# =========================================================
# Proyecto: TABLERO_FIUSAC
# Archivo: 01_explorar_datos.R
# Objetivo: explorar archivos de estudiantes y notas
# =========================================================

rm(list = ls())

required_packages <- c("readr", "readxl", "dplyr", "janitor", "stringr", "purrr", "tibble")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) install.packages(missing)
}

install_if_missing(required_packages)

lapply(required_packages, library, character.only = TRUE)

cat("Directorio de trabajo actual:\n")
print(getwd())

raw_dir <- file.path("data", "raw")

if (!dir.exists(raw_dir)) {
  stop("No existe la carpeta data/raw. Créala y coloca allí los archivos estudiantes.csv y notas.csv")
}

find_file <- function(base_name) {
  candidates <- c(
    file.path(raw_dir, paste0(base_name, ".csv")),
    file.path(raw_dir, paste0(base_name, ".xlsx")),
    file.path(raw_dir, paste0(base_name, ".xls"))
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(NA_character_)
  existing[1]
}

read_generic <- function(path) {
  if (is.na(path)) return(NULL)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(readr::read_csv(path, show_col_types = FALSE))
  }
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    return(readxl::read_excel(path))
  }
  stop("Formato no soportado: ", path)
}

summarize_missing <- function(df) {
  tibble(
    column = names(df),
    missing_n = sapply(df, function(x) sum(is.na(x))),
    missing_pct = round(sapply(df, function(x) mean(is.na(x)) * 100), 2)
  ) %>% arrange(desc(missing_pct))
}

detect_candidate_columns <- function(df) {
  nms <- names(df)
  tibble(
    original_name = nms,
    lower_name = tolower(nms),
    detected_as = case_when(
      stringr::str_detect(lower_name, "^(id|id_estudiante|estudiante_id|carnet)$") ~ "id_estudiante/carnet",
      stringr::str_detect(lower_name, "carnet|registro|matricula") ~ "carnet",
      stringr::str_detect(lower_name, "curso|asignatura|materia|codigo_curso") ~ "curso",
      stringr::str_detect(lower_name, "nota|calificacion|nota_final|promedio") ~ "nota",
      stringr::str_detect(lower_name, "anio|año|year|periodo") ~ "anio",
      stringr::str_detect(lower_name, "ciclo|semestre|trimestre") ~ "ciclo",
      stringr::str_detect(lower_name, "cohorte|ingreso") ~ "cohorte",
      stringr::str_detect(lower_name, "estado|situacion|resultado") ~ "estado",
      stringr::str_detect(lower_name, "area|área|departamento") ~ "area",
      stringr::str_detect(lower_name, "modalidad") ~ "modalidad",
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(detected_as))
}

students_path <- find_file("estudiantes")
grades_path   <- find_file("notas")

cat("\nArchivo detectado para estudiantes:\n")
print(students_path)

cat("\nArchivo detectado para notas:\n")
print(grades_path)

students <- read_generic(students_path)
grades   <- read_generic(grades_path)

if (is.null(students)) warning("No se encontró archivo de estudiantes")
if (is.null(grades)) warning("No se encontró archivo de notas")

if (!is.null(students)) {
  students <- janitor::clean_names(students)
  cat("\n==============================\n")
  cat("ESTUDIANTES - Primeras 5 filas\n")
  cat("==============================\n")
  print(head(students, 5))
  
  cat("\nColumnas de estudiantes:\n")
  print(names(students))
  
  cat("\nEstructura de estudiantes:\n")
  str(students)
  
  cat("\nResumen de missing values - estudiantes:\n")
  print(summarize_missing(students))
  
  cat("\nColumnas candidatas detectadas - estudiantes:\n")
  print(detect_candidate_columns(students))
}

if (!is.null(grades)) {
  grades <- janitor::clean_names(grades)
  cat("\n=========================\n")
  cat("NOTAS - Primeras 5 filas\n")
  cat("=========================\n")
  print(head(grades, 5))
  
  cat("\nColumnas de notas:\n")
  print(names(grades))
  
  cat("\nEstructura de notas:\n")
  str(grades)
  
  cat("\nResumen de missing values - notas:\n")
  print(summarize_missing(grades))
  
  cat("\nColumnas candidatas detectadas - notas:\n")
  print(detect_candidate_columns(grades))
}

cat("\nExploración finalizada correctamente.\n")
