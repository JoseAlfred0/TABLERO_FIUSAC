# Helpers para importación robusta de datos FIUSAC

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(purrr)
  library(tibble)
})

project_root <- function() {
  getwd()
}

find_data_directory <- function() {
  data_dir <- file.path(project_root(), "data", "raw")
  if (!dir.exists(data_dir)) {
    stop("No se encontró carpeta de datos esperada en ruta relativa: data/raw")
  }
  data_dir
}

locate_dataset_file <- function(data_dir, stem) {
  files <- list.files(data_dir, full.names = TRUE)
  valid_ext <- c("csv", "xlsx", "xls")
  pattern <- paste0("(?i)", stem, "\\.((", paste(valid_ext, collapse = "|"), "))$")
  hit <- files[str_detect(basename(files), pattern)]

  if (length(hit) == 0) {
    fallback <- files[str_detect(tolower(basename(files)), paste0("^", stem))]
    if (length(fallback) == 0) {
      stop(paste0("No se encontró archivo para: ", stem, " en ", data_dir))
    }
    hit <- fallback[[1]]
  } else {
    hit <- hit[[1]]
  }

  hit
}

read_tabular_auto <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE, guess_max = 50000))
  }

  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(path))
  }

  stop(paste0("Formato no soportado: ", ext, " (", path, ")"))
}

detect_key_candidates <- function(df) {
  nms <- names(df)
  nms_clean <- nms |>
    janitor::make_clean_names() |>
    stringr::str_replace_all("_", "")

  rules <- list(
    id_estudiante = c("idestudiante", "idalumno", "idpersona", "id", "codigoestudiante", "registro"),
    carnet = c("carnet", "carne", "registroacademico", "registro"),
    estudiante = c("estudiante", "alumno", "nombre", "nombres", "estudiante_nombre"),
    curso = c("curso", "asignatura", "materia", "codigo", "codigocurso", "nombrecurso"),
    nota = c("nota", "notafinal", "promedio", "calificacion", "score", "grade"),
    anio = c("anio", "ano", "year", "periodo", "periodo_anio", "academicyear"),
    ciclo = c("ciclo", "semestre", "termino", "periodo", "bimestre", "trimestre"),
    cohorte = c("cohorte", "cohort", "anioingreso", "periodoingreso", "ingreso"),
    estado = c("estado", "resultado", "situacion", "condicion", "status", "estadoacademico", "retirado"),
    area = c("area", "departamento", "eje", "categoria", "linea"),
    modalidad = c("modalidad", "jornada", "plan", "sede", "metodologia")
  )

  map(rules, function(patterns) {
    idx <- map_lgl(nms_clean, ~ any(stringr::str_detect(.x, patterns)))
    nms[idx]
  })
}

column_profile <- function(df) {
  tibble(
    columna = names(df),
    tipo = map_chr(df, ~ paste(class(.x), collapse = "/")),
    n_missing = map_int(df, ~ sum(is.na(.x))),
    pct_missing = round(map_dbl(df, ~ mean(is.na(.x)) * 100), 2),
    n_unicos = map_int(df, ~ dplyr::n_distinct(.x, na.rm = TRUE))
  )
}

print_exploration_summary <- function(df, title = "dataset") {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("Resumen exploratorio:", title, "\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat("Filas:", nrow(df), " | Columnas:", ncol(df), "\n\n")

  cat("Primeras 5 filas:\n")
  print(utils::head(df, 5))

  cat("\nNombres de columnas:\n")
  print(names(df))

  cat("\nEstructura:\n")
  str(df)

  cat("\nTipos y missing por columna:\n")
  print(column_profile(df))

  cat("\nColumnas candidatas por variable clave:\n")
  candidates <- detect_key_candidates(df)
  for (nm in names(candidates)) {
    vals <- candidates[[nm]]
    cat(" -", nm, ":", ifelse(length(vals) == 0, "[sin coincidencias]", paste(vals, collapse = ", ")), "\n")
  }

  invisible(candidates)
}
