#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readxl)
  library(readr)
})

source(file.path("R", "helpers_data_import.R"))

dir.create("docs", showWarnings = FALSE, recursive = TRUE)

data_dir <- find_data_directory()
cat("Directorio de datos detectado:", data_dir, "\n")

estudiantes_path <- locate_dataset_file(data_dir, "estudiantes")
notas_path <- locate_dataset_file(data_dir, "notas")

cat("Archivo estudiantes:", estudiantes_path, "\n")
cat("Archivo notas:", notas_path, "\n")

estudiantes <- read_tabular_auto(estudiantes_path)
notas <- read_tabular_auto(notas_path)

cand_est <- print_exploration_summary(estudiantes, "estudiantes")
cand_not <- print_exploration_summary(notas, "notas")

profile_est <- column_profile(estudiantes)
profile_not <- column_profile(notas)

issues <- tibble(
  dataset = c("estudiantes", "notas"),
  filas = c(nrow(estudiantes), nrow(notas)),
  columnas = c(ncol(estudiantes), ncol(notas)),
  pct_missing_global = c(
    round(mean(is.na(estudiantes)) * 100, 2),
    round(mean(is.na(notas)) * 100, 2)
  )
)

render_candidates <- function(x) {
  lines <- map2_chr(names(x), x, ~ paste0("- ", .x, ": ", ifelse(length(.y) == 0, "[sin coincidencias]", paste(.y, collapse = ", "))))
  paste(lines, collapse = "\n")
}

report <- glue::glue(
"# Resumen exploratorio de datos FIUSAC

**Fecha de ejecución:** {Sys.time()}  
**Directorio de datos:** `{data_dir}`

## Archivos detectados
- estudiantes: `{estudiantes_path}`
- notas: `{notas_path}`

## Resumen de calidad general

{paste(capture.output(print(issues)), collapse = '\n')}

## Candidatos de columnas clave (estudiantes)
{render_candidates(cand_est)}

## Candidatos de columnas clave (notas)
{render_candidates(cand_not)}

## Missing values por columna - estudiantes

{paste(capture.output(print(profile_est)), collapse = '\n')}

## Missing values por columna - notas

{paste(capture.output(print(profile_not)), collapse = '\n')}
")

writeLines(report, "docs/resumen_exploracion.md")
cat("\nReporte guardado en docs/resumen_exploracion.md\n")

cat("\nSiguiente paso sugerido: ejecutar R/02_limpieza_y_etl.R\n")
