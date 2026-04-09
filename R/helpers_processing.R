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

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
    identificador = first_existing(df, c("identificador")),
    carrera = first_existing(df, c("carrera")),
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

# =========================================================
# Llave compuesta oficial del proyecto
# =========================================================
get_explicit_join_keys <- function() {
  c("identificador", "carrera")
}

validate_explicit_join_keys <- function(estudiantes, notas, join_keys = get_explicit_join_keys()) {
  missing_students <- setdiff(join_keys, names(estudiantes))
  missing_notas <- setdiff(join_keys, names(notas))
  
  if (length(missing_students) > 0) {
    stop(
      paste0(
        "Faltan columnas requeridas en estudiantes: ",
        paste(missing_students, collapse = ", ")
      )
    )
  }
  
  if (length(missing_notas) > 0) {
    stop(
      paste0(
        "Faltan columnas requeridas en notas: ",
        paste(missing_notas, collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}

normalize_numeric_grade <- function(x) {
  if (is.numeric(x)) return(x)
  
  x |>
    stringr::str_replace_all(",", ".") |>
    readr::parse_number(locale = locale(decimal_mark = "."))
}

normalize_year <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

normalize_cycle_text <- function(x) {
  x_chr <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
  
  dplyr::case_when(
    stringr::str_detect(x_chr, "PRIMER") ~ "1",
    stringr::str_detect(x_chr, "SEGUNDO") ~ "2",
    stringr::str_detect(x_chr, "VACACIONES") ~ "V",
    x_chr %in% c("1", "2", "I", "II", "V") ~ x_chr,
    TRUE ~ x_chr
  )
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

# =========================================================
# Diagnóstico de cardinalidad entre tablas
# =========================================================
diagnose_join_cardinality <- function(estudiantes, notas, join_keys = get_explicit_join_keys()) {
  students_dup <- estudiantes |>
    count(across(all_of(join_keys)), name = "n_est") |>
    filter(n_est > 1)
  
  notes_dup <- notas |>
    count(across(all_of(join_keys)), name = "n_notas") |>
    filter(n_notas > 1)
  
  n_students_dup <- nrow(students_dup)
  n_notes_dup <- nrow(notes_dup)
  
  relation <- dplyr::case_when(
    n_students_dup == 0 & n_notes_dup == 0 ~ "one-to-one",
    n_students_dup == 0 & n_notes_dup > 0 ~ "one-to-many",
    n_students_dup > 0 & n_notes_dup == 0 ~ "many-to-one",
    TRUE ~ "many-to-many"
  )
  
  list(
    relation = relation,
    duplicated_keys_estudiantes = n_students_dup,
    duplicated_keys_notas = n_notes_dup
  )
}

# =========================================================
# Construcción de tabla maestra
# =========================================================
build_master_table <- function(estudiantes_raw, notas_raw) {
  estudiantes <- standardize_df(estudiantes_raw)
  notas <- standardize_df(notas_raw)
  
  # En notas, la carrera viene como estcarr
  if ("estcarr" %in% names(notas) && !"carrera" %in% names(notas)) {
    notas <- notas |>
      rename(carrera = estcarr)
  }
  
  join_keys <- get_explicit_join_keys()
  validate_explicit_join_keys(estudiantes, notas, join_keys)
  
  cardinality <- diagnose_join_cardinality(estudiantes, notas, join_keys)
  
  if (identical(cardinality$relation, "many-to-many")) {
    warning(
      paste0(
        "Se detectó una relación many-to-many en la unión por [",
        paste(join_keys, collapse = ", "),
        "]. ",
        "Duplicados en estudiantes: ", cardinality$duplicated_keys_estudiantes,
        ". Duplicados en notas: ", cardinality$duplicated_keys_notas, "."
      ),
      call. = FALSE
    )
  }
  
  joined <- notas |>
    left_join(estudiantes, by = join_keys, suffix = c("_notas", "_est"))
  
  cmap <- infer_column_map(joined)
  
  nota_col <- cmap$nota_final
  estado_col <- cmap$estado
  anio_col <- cmap$anio_academico
  ciclo_col <- cmap$ciclo
  cohorte_col <- cmap$cohorte
  
  joined <- joined |>
    mutate(
      nota_final = if (!is.na(nota_col)) normalize_numeric_grade(.data[[nota_col]]) else NA_real_,
      anio_academico = if (!is.na(anio_col)) normalize_year(.data[[anio_col]]) else NA_real_,
      ciclo = if (!is.na(ciclo_col)) normalize_cycle_text(.data[[ciclo_col]]) else NA_character_,
      cohorte = if (!is.na(cohorte_col)) as.character(.data[[cohorte_col]]) else NA_character_,
      estado_academico = classify_status(
        nota_final,
        if (!is.na(estado_col)) .data[[estado_col]] else NULL
      )
    ) |>
    mutate(anio_academico = as.integer(anio_academico))
  
  attr(joined, "join_key") <- join_keys
  attr(joined, "column_map") <- cmap
  attr(joined, "join_cardinality") <- cardinality
  
  joined
}

# =========================================================
# Validaciones de calidad sobre la tabla maestra
# =========================================================
validate_master <- function(df, join_key) {
  problems <- list()
  
  if (!all(join_key %in% names(df))) {
    missing_keys <- setdiff(join_key, names(df))
    stop(
      paste0(
        "Faltan columnas de llave en la tabla maestra: ",
        paste(missing_keys, collapse = ", ")
      )
    )
  }
  
  # Llaves vacías considerando la llave compuesta
  problems$llaves_vacias <- df |>
    mutate(
      llave_vacia = if_any(all_of(join_key), ~ is.na(.) | as.character(.) == "")
    ) |>
    summarise(n = sum(llave_vacia), .groups = "drop") |>
    pull(n)
  
  # Duplicados por llave compuesta
  problems$duplicados <- df |>
    count(across(all_of(join_key)), name = "n") |>
    filter(n > 1) |>
    nrow()
  
  problems$notas_fuera_rango <- sum(df$nota_final < 0 | df$nota_final > 100, na.rm = TRUE)
  
  problems$anios_fuera_rango <- sum(
    !is.na(df$anio_academico) & (df$anio_academico < 2001 | df$anio_academico > 2024),
    na.rm = TRUE
  )
  
  if ("ciclo" %in% names(df)) {
    cycle_vals <- tolower(unique(na.omit(as.character(df$ciclo))))
    bad_cycle <- cycle_vals[!str_detect(cycle_vals, "^(1|2|v|i|ii|semestre 1|semestre 2|primer|segundo|vacaciones.*)$")]
  } else {
    bad_cycle <- character(0)
  }
  
  problems$ciclos_inconsistentes <- length(bad_cycle)
  problems$detalle_ciclos_inconsistentes <- bad_cycle
  
  problems
}