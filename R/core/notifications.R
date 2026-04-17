notify_user <- function(mensaje, tipo = "default", duracion = 5) {
  
  tipos_validos <- c("default", "message", "warning", "error")
  
  if (!tipo %in% tipos_validos) tipo <- "default"
  
  shiny::showNotification(mensaje, type = tipo, duration = duracion)
}
