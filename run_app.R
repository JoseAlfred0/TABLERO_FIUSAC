list.files("data/processed")

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Instale shiny. Ejecute renv::restore() primero.")
}

shiny::runApp(appDir = "app", launch.browser = TRUE)
