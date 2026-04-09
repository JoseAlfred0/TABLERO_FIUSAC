suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(janitor)
  library(lubridate)
})

# =========================================================
# Carga de helpers y módulos
# La app corre desde la carpeta /app, por eso:
# - para /R se usa ../R
# - para /data se usa ../data
# - para módulos dentro de /app/modules se usa modules
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
# Normalización defensiva de la tabla maestra
# =========================================================

normalize_master_data <- function(df) {
  out <- df
  
  # Alias comunes por si algún nombre varió en ETL
  if ("course" %in% names(out) && !"curso" %in% names(out)) {
    out <- out |> rename(curso = course)
  }
  
  if ("final_grade" %in% names(out) && !"nota_final" %in% names(out)) {
    out <- out |> rename(nota_final = final_grade)
  }
  
  if ("estado_final" %in% names(out) && !"estado_academico" %in% names(out)) {
    out <- out |> rename(estado_academico = estado_final)
  }
  
  if ("cohort" %in% names(out) && !"cohorte" %in% names(out)) {
    out <- out |> rename(cohorte = cohort)
  }
  
  if ("year" %in% names(out) && !"anio_academico" %in% names(out)) {
    out <- out |> rename(anio_academico = year)
  }
  
  # Tipificación defensiva
  if ("anio_academico" %in% names(out)) {
    out <- out |> mutate(anio_academico = suppressWarnings(as.integer(anio_academico)))
  }
  
  if ("cohorte" %in% names(out)) {
    out <- out |> mutate(cohorte = suppressWarnings(as.integer(cohorte)))
  }
  
  if ("nota_final" %in% names(out)) {
    out <- out |> mutate(nota_final = suppressWarnings(as.numeric(nota_final)))
  }
  
  if ("ciclo" %in% names(out)) {
    out <- out |> mutate(ciclo = as.character(ciclo))
  }
  
  if ("curso" %in% names(out)) {
    out <- out |> mutate(curso = as.character(curso))
  }
  
  if ("carrera" %in% names(out)) {
    out <- out |> mutate(carrera = as.character(carrera))
  }
  
  if ("area" %in% names(out)) {
    out <- out |> mutate(area = as.character(area))
  }
  
  if ("modalidad" %in% names(out)) {
    out <- out |> mutate(modalidad = as.character(modalidad))
  }
  
  if ("estado_academico" %in% names(out)) {
    out <- out |> mutate(estado_academico = as.character(estado_academico))
  }
  
  out
}

# =========================================================
# Aplicación de filtros
# =========================================================

apply_filters <- function(df, filters) {
  out <- df
  
  if (!is.null(filters$anio) && length(filters$anio) > 0 && "anio_academico" %in% names(out)) {
    out <- out |> filter(anio_academico %in% suppressWarnings(as.integer(filters$anio)))
  }
  
  if (!is.null(filters$ciclo) && length(filters$ciclo) > 0 && "ciclo" %in% names(out)) {
    out <- out |> filter(ciclo %in% filters$ciclo)
  }
  
  if (!is.null(filters$cohorte) && length(filters$cohorte) > 0 && "cohorte" %in% names(out)) {
    out <- out |> filter(cohorte %in% suppressWarnings(as.integer(filters$cohorte)))
  }
  
  if (!is.null(filters$curso) && length(filters$curso) > 0 && "curso" %in% names(out)) {
    out <- out |> filter(curso %in% filters$curso)
  }
  
  if (!is.null(filters$area) && length(filters$area) > 0 && "area" %in% names(out)) {
    out <- out |> filter(area %in% filters$area)
  }
  
  if (!is.null(filters$carrera) && length(filters$carrera) > 0 && "carrera" %in% names(out)) {
    out <- out |> filter(carrera %in% filters$carrera)
  }
  
  if (!is.null(filters$modalidad) && length(filters$modalidad) > 0 && "modalidad" %in% names(out)) {
    out <- out |> filter(modalidad %in% filters$modalidad)
  }
  
  out
}

# =========================================================
# Carga global de datos
# =========================================================

datos_fiusac <- load_master_data() |> normalize_master_data()

# =========================================================
# Vectores auxiliares para controles UI
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

carreras_disponibles <- if ("carrera" %in% names(datos_fiusac)) {
  datos_fiusac |>
    distinct(carrera) |>
    filter(!is.na(carrera)) |>
    arrange(carrera) |>
    pull(carrera)
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