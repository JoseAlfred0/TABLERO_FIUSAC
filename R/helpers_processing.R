# Helpers de limpieza y ETL FIUSAC

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(purrr)
  library(readr)
})

source(file.path("R", "helpers_data_import.R"))

first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

standardize_df <- function(df) {
  df |>
    janitor::clean_names() |>
    mutate(across(where(is.character), ~ stringr::str_squish(.x)))
}

infer_column_map <- function(df) {
  candidates <- detect_key_candidates(df)

  list(
    id_estudiante = first_existing(df, janitor::make_clean_names(candidates$id_estudiante)),
    carnet = first_existing(df, janitor::make_clean_names(candidates$carnet)),
    curso = first_existing(df, janitor::make_clean_names(candidates$curso)),
    nota_final = first_existing(df, janitor::make_clean_names(candidates$nota)),
    anio_academico = first_existing(df, janitor::make_clean_names(candidates$anio)),
    ciclo = first_existing(df, janitor::make_clean_names(candidates$ciclo)),
    cohorte = first_existing(df, janitor::make_clean_names(candidates$cohorte)),
    estado = first_existing(df, janitor::make_clean_names(candidates$estado)),
    area = first_existing(df, janitor::make_clean_names(candidates$area)),
    modalidad = first_existing(df, janitor::make_clean_names(candidates$modalidad))
  )
}

coalesce_join_key <- function(estudiantes, notas) {
  keys_priority <- c("id_estudiante", "carnet", "registro", "codigo_estudiante", "id")
  commons <- intersect(names(estudiantes), names(notas))
  keys <- keys_priority[keys_priority %in% commons]

  if (length(keys) == 0) {
    stop("No hay llave de unión común evidente entre estudiantes y notas.")
  }

  keys[[1]]
}

normalize_numeric_grade <- function(x) {
  if (is.numeric(x)) return(x)

  x |>
    stringr::str_replace_all(",", ".") |>
    readr::parse_number(locale = locale(decimal_mark = "."))
}

classify_status <- function(nota_final, estado_raw = NULL) {
  estado_raw <- tolower(as.character(estado_raw %||% ""))
  is_retiro <- stringr::str_detect(estado_raw, "reti|aband|desert")

  dplyr::case_when(
    is_retiro ~ "Retirado",
    is.na(nota_final) ~ NA_character_,
    nota_final >= 61 ~ "Aprobado",
    nota_final < 61 ~ "Reprobado",
    TRUE ~ NA_character_
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

build_master_table <- function(estudiantes_raw, notas_raw) {
  estudiantes <- standardize_df(estudiantes_raw)
  notas <- standardize_df(notas_raw)

  join_key <- coalesce_join_key(estudiantes, notas)

  joined <- notas |>
    left_join(estudiantes, by = join_key, suffix = c("_notas", "_est"))

  cmap <- infer_column_map(joined)

  nota_col <- cmap$nota_final
  estado_col <- cmap$estado
  anio_col <- cmap$anio_academico
  ciclo_col <- cmap$ciclo
  cohorte_col <- cmap$cohorte

  joined <- joined |>
    mutate(
      nota_final = if (!is.na(nota_col)) normalize_numeric_grade(.data[[nota_col]]) else NA_real_,
      anio_academico = if (!is.na(anio_col)) readr::parse_number(as.character(.data[[anio_col]])) else NA_real_,
      ciclo = if (!is.na(ciclo_col)) as.character(.data[[ciclo_col]]) else NA_character_,
      cohorte = if (!is.na(cohorte_col)) as.character(.data[[cohorte_col]]) else NA_character_,
      estado_academico = classify_status(
        nota_final,
        if (!is.na(estado_col)) .data[[estado_col]] else NULL
      )
    ) |>
    mutate(anio_academico = as.integer(anio_academico))

  attr(joined, "join_key") <- join_key
  attr(joined, "column_map") <- cmap
  joined
}

validate_master <- function(df, join_key) {
  problems <- list()

  if (join_key %in% names(df)) {
    problems$llaves_vacias <- sum(is.na(df[[join_key]]) | df[[join_key]] == "")
  } else {
    problems$llaves_vacias <- NA_integer_
  }

  problems$duplicados <- df |>
    count(.data[[join_key]]) |>
    filter(n > 1, !is.na(.data[[join_key]])) |>
    nrow()

  problems$notas_fuera_rango <- sum(df$nota_final < 0 | df$nota_final > 100, na.rm = TRUE)

  problems$anios_fuera_rango <- sum(
    !is.na(df$anio_academico) & (df$anio_academico < 2001 | df$anio_academico > 2024),
    na.rm = TRUE
  )

  cycle_vals <- tolower(unique(na.omit(as.character(df$ciclo))))
  bad_cycle <- cycle_vals[!str_detect(cycle_vals, "^(1|2|i|ii|semestre 1|semestre 2|primer|segundo)$")]
  problems$ciclos_inconsistentes <- length(bad_cycle)
  problems$detalle_ciclos_inconsistentes <- bad_cycle

  problems
}
