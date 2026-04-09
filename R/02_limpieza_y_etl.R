#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
})

source(file.path("R", "helpers_data_import.R"))
source(file.path("R", "helpers_processing.R"))
source(file.path("R", "helpers_metrics.R"))

# =========================================================
# 1. Preparación de directorios
# =========================================================
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("docs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# =========================================================
# 2. Lectura de datos crudos
# =========================================================
data_dir <- find_data_directory()

estudiantes <- read_tabular_auto(locate_dataset_file(data_dir, "estudiantes"))
notas <- read_tabular_auto(locate_dataset_file(data_dir, "notas"))

# =========================================================
# 3. Construcción de tabla maestra
#    La lógica de unión vive en helpers_processing.R
# =========================================================
master <- build_master_table(estudiantes, notas)

join_key <- attr(master, "join_key")
column_map <- attr(master, "column_map")

# Asegurar estructura consistente de join_key
if (is.null(join_key)) {
  stop("No se encontró el atributo 'join_key' en la tabla maestra.")
}

if (!is.character(join_key)) {
  stop("El atributo 'join_key' debe ser de tipo character.")
}

join_key_label <- paste(join_key, collapse = " + ")

# =========================================================
# 4. Validaciones de calidad
# =========================================================
validaciones <- validate_master(master, join_key)

# =========================================================
# 5. Exportación de tabla maestra
# =========================================================
saveRDS(master, "data/processed/datos_fiusac_procesados.rds")

# =========================================================
# 6. Métricas base
# =========================================================
base_metrics <- calculate_base_metrics(master)
readr::write_csv(base_metrics, "outputs/tables/indicadores_base_por_periodo.csv")

# =========================================================
# 7. Deserción proxy
#    La deserción proxy debe calcularse por estudiante,
#    no por llave compuesta estudiante+carrera.
# =========================================================
possible_id <- "identificador"

if (!possible_id %in% names(master)) {
  warning("No existe la columna 'identificador' en la tabla maestra. No se calculará deserción proxy.")
  dropout_proxy <- tibble()
} else {
  dropout_proxy <- calculate_dropout_proxy(master, id_col = possible_id)
}

readr::write_csv(dropout_proxy, "outputs/tables/desercion_proxy_por_anio.csv")

# =========================================================
# 8. Advertencias operativas
# =========================================================
warn_lines <- c()

if (!"cohorte" %in% names(master) || all(is.na(master$cohorte))) {
  warn_lines <- c(warn_lines, "- No se detectó cohorte usable para indicadores por cohorte.")
}

if (!"modalidad" %in% names(master)) {
  warn_lines <- c(warn_lines, "- No se detectó modalidad; los filtros por modalidad quedarán desactivados en la app.")
}

if (!"area" %in% names(master)) {
  warn_lines <- c(warn_lines, "- No se detectó área académica; algunos análisis por área pueden quedar limitados.")
}

if (nrow(dropout_proxy) == 0) {
  warn_lines <- c(warn_lines, "- No fue posible calcular deserción proxy por falta de identificador estable o año.")
}

if (length(warn_lines) == 0) {
  warn_lines <- "- Sin advertencias críticas para ETL."
}

# =========================================================
# 9. Diccionario / documentación técnica
# =========================================================
mapping_lines <- paste0(
  "- ", names(column_map), ": `",
  ifelse(is.na(unlist(column_map)), "NA", unlist(column_map)),
  "`"
)

writeLines(
  c(
    "# Diccionario de datos (versión inicial)",
    "",
    "## Llave de unión seleccionada",
    paste0("- `", join_key_label, "` (llave explícita definida para evitar uniones ambiguas)."),
    "- En la tabla de notas, si existe `estcarr`, se renombra a `carrera` antes de la unión.",
    "- La unión debe reflejar la relación entre estudiante y carrera, no solo estudiante.",
    "",
    "## Mapeo de columnas detectadas",
    mapping_lines,
    "",
    "## Nuevas columnas derivadas",
    "- `nota_final`: nota normalizada numérica.",
    "- `anio_academico`: año académico inferido y convertido a entero.",
    "- `ciclo`: periodo académico estandarizado como texto.",
    "- `cohorte`: cohorte inferida cuando existe.",
    "- `estado_academico`: Aprobado/Reprobado/Retirado según regla de negocio.",
    "",
    "## Reglas de transformación",
    "- Aprobado si `nota_final >= 61`.",
    "- Reprobado si `nota_final < 61`.",
    "- Retirado si el campo de estado original contiene patrones de retiro/abandono/deserción.",
    "",
    "## Validaciones de calidad",
    paste0("- Llaves vacías: ", validaciones$llaves_vacias),
    paste0("- Duplicados por llave: ", validaciones$duplicados),
    paste0("- Notas fuera de rango [0,100]: ", validaciones$notas_fuera_rango),
    paste0("- Años fuera de rango [2001,2024]: ", validaciones$anios_fuera_rango),
    paste0("- Ciclos inconsistentes: ", validaciones$ciclos_inconsistentes),
    if (length(validaciones$detalle_ciclos_inconsistentes) > 0) {
      paste0("- Valores de ciclo no estándar: ", paste(validaciones$detalle_ciclos_inconsistentes, collapse = ", "))
    } else {
      "- Valores de ciclo no estándar: ninguno"
    },
    "",
    "## Advertencias",
    warn_lines
  ),
  "docs/diccionario_datos.md"
)

# =========================================================
# 10. Salida en consola
# =========================================================
cat("ETL finalizado. Archivo maestro: data/processed/datos_fiusac_procesados.rds\n")
cat("Diccionario actualizado en docs/diccionario_datos.md\n")
cat("Llave de unión utilizada: ", join_key_label, "\n", sep = "")