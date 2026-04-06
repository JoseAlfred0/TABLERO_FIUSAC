suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(janitor)
  library(lubridate)
})

source(file.path("R", "helpers_metrics.R"))
source(file.path("app", "modules", "mod_filters.R"))
source(file.path("app", "modules", "mod_kpis.R"))
source(file.path("app", "modules", "mod_timeseries.R"))
source(file.path("app", "modules", "mod_boxplots.R"))
source(file.path("app", "modules", "mod_heatmap.R"))
source(file.path("app", "modules", "mod_top_courses.R"))

load_master_data <- function(path = "data/processed/datos_fiusac_procesados.rds") {
  if (!file.exists(path)) {
    stop("No existe el archivo procesado. Ejecute R/02_limpieza_y_etl.R primero.")
  }
  readRDS(path)
}

apply_filters <- function(df, filters) {
  out <- df
  
  if (!is.null(filters$anio) && length(filters$anio) > 0) out <- out |> filter(anio_academico %in% as.integer(filters$anio))
  if (!is.null(filters$ciclo) && length(filters$ciclo) > 0) out <- out |> filter(ciclo %in% filters$ciclo)
  if (!is.null(filters$cohorte) && length(filters$cohorte) > 0) out <- out |> filter(cohorte %in% filters$cohorte)
  if (!is.null(filters$curso) && length(filters$curso) > 0) out <- out |> filter(curso %in% filters$curso)
  if (!is.null(filters$area) && length(filters$area) > 0) out <- out |> filter(area %in% filters$area)
  if ("modalidad" %in% names(out) && !is.null(filters$modalidad) && length(filters$modalidad) > 0) out <- out |> filter(modalidad %in% filters$modalidad)
  
  out
}
