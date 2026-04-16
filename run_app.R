# run_app.R — Ejecutar la app desde cualquier subcarpeta
options(shiny.autoreload = FALSE)

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Falta el paquete 'shiny'. Instálalo con install.packages('shiny').")
}

# Buscar la raíz del proyecto caminando hacia arriba
encontrar_raiz <- function(max_niveles = 8) {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidatos <- c(wd)
  for (i in 1:max_niveles) {
    wd <- normalizePath(file.path(wd, ".."), winslash = "/", mustWork = TRUE)
    candidatos <- c(candidatos, wd)
  }
  for (p in candidatos) {
    if (file.exists(file.path(p, "03_shiny_app", "app.R")) &&
        file.exists(file.path(p, "run_app.R"))) {
      return(p)
    }
  }
  return(NULL)
}

raiz <- encontrar_raiz()
if (is.null(raiz)) {
  stop("No se encontró la raíz del proyecto. Asegúrate de ejecutar desde dentro de la carpeta del proyecto.")
}

setwd(raiz)
message("Raíz detectada: ", raiz)
shiny::runApp("03_shiny_app", launch.browser = TRUE)
