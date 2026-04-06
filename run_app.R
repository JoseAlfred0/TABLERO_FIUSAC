source("R/02_limpieza_y_etl.R")#!/usr/bin/env Rscript

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Instale shiny. Ejecute renv::restore() primero.")
}

shiny::runApp(appDir = "app", launch.browser = TRUE)
