suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(bslib)
  library(janitor)
  library(lubridate)
})

# =========================================================
# Carga de funciones auxiliares y módulos
# Nota:
# Como la app se ejecuta desde la carpeta "app",
# para acceder a la carpeta raíz del proyecto se usa ".."
# =========================================================

source(file.path("..", "R", "helpers_metrics.R"))
source(file.path("modules", "mod_filters.R"))
source(file.path("modules", "mod_kpis.R"))
source(file.path("modules", "mod_timeseries.R"))
source(file.path("modules", "mod_boxplots.R"))
source(file.path("modules", "mod_heatmap.R"))
source(file.path("modules", "mod_top_courses.R"))

# =========================================================
# Función para cargar la tabla maestra procesada
# =========================================================

load_master_data <- function(path = file.path("..", "data", "processed", "datos_fiusac_procesados.rds")) {
  if (!file.exists(path)) {
    stop("No existe el archivo procesado. Ejecute R/02_limpieza_y_etl.R primero.")
  }
  readRDS(path)
}

# =========================================================
# Aplicación de filtros
# =========================================================

apply_filters <- function(df, filters) {
  out <- df
  
  if (!is.null(filters$anio) && length(filters$anio) > 0 && "anio_academico" %in% names(out)) {
    out <- out |> filter(anio_academico %in% as.integer(filters$anio))
  }
  
  if (!is.null(filters$ciclo) && length(filters$ciclo) > 0 && "ciclo" %in% names(out)) {
    out <- out |> filter(ciclo %in% filters$ciclo)
  }
  
  if (!is.null(filters$cohorte) && length(filters$cohorte) > 0 && "cohorte" %in% names(out)) {
    out <- out |> filter(cohorte %in% filters$cohorte)
  }
  
  if (!is.null(filters$curso) && length(filters$curso) > 0 && "curso" %in% names(out)) {
    out <- out |> filter(curso %in% filters$curso)
  }
  
  if (!is.null(filters$area) && length(filters$area) > 0 && "area" %in% names(out)) {
    out <- out |> filter(area %in% filters$area)
  }
  
  if (!is.null(filters$modalidad) && length(filters$modalidad) > 0 && "modalidad" %in% names(out)) {
    out <- out |> filter(modalidad %in% filters$modalidad)
  }
  
  out
}

# =========================================================
# Cargar datos globales
# =========================================================

datos_fiusac <- load_master_data()

# =========================================================
# Normalización ligera de nombres esperados por la app
# Esto ayuda si en ETL quedaron nombres como:
# - course -> curso
# - cohort -> cohorte
# - student_id -> carnet
# =========================================================

if ("course" %in% names(datos_fiusac) && !"curso" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |> rename(curso = course)
}

if ("cohort" %in% names(datos_fiusac) && !"cohorte" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |> rename(cohorte = cohort)
}

if ("student_id" %in% names(datos_fiusac) && !"carnet" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |> rename(carnet = student_id)
}

if ("final_grade" %in% names(datos_fiusac) && !"nota_final" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |> rename(nota_final = final_grade)
}

# =========================================================
# Conversión defensiva de tipos
# =========================================================

if ("anio_academico" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |>
    mutate(anio_academico = suppressWarnings(as.integer(anio_academico)))
}

if ("cohorte" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |>
    mutate(cohorte = suppressWarnings(as.integer(cohorte)))
}

if ("nota_final" %in% names(datos_fiusac)) {
  datos_fiusac <- datos_fiusac |>
    mutate(nota_final = suppressWarnings(as.numeric(nota_final)))
}

# =========================================================
# Objetos auxiliares para selects/filtros
# =========================================================

anios_disponibles <- if ("anio_academico" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(anio_academico) |>
    filter(!is.na(anio_academico)) |>
    arrange(anio_academico) |>
    pull(anio_academico)
} else {
  integer(0)
}

ciclos_disponibles <- if ("ciclo" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(ciclo) |>
    filter(!is.na(ciclo)) |>
    arrange(ciclo) |>
    pull(ciclo)
} else {
  character(0)
}

cohortes_disponibles <- if ("cohorte" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(cohorte) |>
    filter(!is.na(cohorte)) |>
    arrange(cohorte) |>
    pull(cohorte)
} else {
  integer(0)
}

cursos_disponibles <- if ("curso" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(curso) |>
    filter(!is.na(curso)) |>
    arrange(curso) |>
    pull(curso)
} else {
  character(0)
}

areas_disponibles <- if ("area" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(area) |>
    filter(!is.na(area)) |>
    arrange(area) |>
    pull(area)
} else {
  character(0)
}

modalidades_disponibles <- if ("modalidad" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(modalidad) |>
    filter(!is.na(modalidad)) |>
    arrange(modalidad) |>
    pull(modalidad)
} else {
  character(0)
}