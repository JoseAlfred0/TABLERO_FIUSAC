verificar_requisitos <- function() {
  
  log_message("titulo", "VERIFICACIÓN DE REQUISITOS - TABLERO FIUSAC V7.0")
  
  if (!dir.exists("datos")) {
    dir.create("datos", recursive = TRUE)
    log_message("info", "Creada carpeta 'datos/'")
  }
  
  archivo_datos <- resolver_ruta_datos()
  
  if (!file.exists(archivo_datos)) {
    log_message("error", paste("Archivo de datos no encontrado:", archivo_datos))
    log_message("info", "Coloque 'datos_2001_2024.csv' en '01_data/' o en '03_shiny_app/datos/'")
    return(FALSE)
  }
  
  paquetes <- c("shiny","shinydashboard","dplyr","data.table","plotly","DT")
  faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
  
  if (length(faltantes) > 0) {
    log_message("warning", paste("Faltan paquetes:", paste(faltantes, collapse=", ")))
    return(FALSE)
  }
  
  log_message("info", "✅ Requisitos verificados")
  return(TRUE)
}