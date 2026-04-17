resolver_ruta_datos <- function() {
  
  # Prioridad 1: proyecto robusto
  p1 <- file.path("..", "01_data", "datos_2001_2024.csv")
  if (file.exists(p1)) return(p1)
  
  # Compatibilidad 2: estructura anterior
  p2 <- file.path("datos", "datos_2001_2024.csv")
  if (file.exists(p2)) return(p2)
  
  return(p1)
}

resolver_ruta_complementarios <- function() {
  
  candidatos <- c(
    file.path("datos", "datos_complementarios_limpios.csv"),
    file.path("datos", "datos_complementarios.csv"),
    file.path("datos", "datos_complementarios - copia.csv")
  )
  
  if (dir.exists("datos")) {
    extra <- list.files("datos", pattern = "^datos_complementarios.*\\.csv$", full.names = TRUE)
    if (length(extra) > 0) {
      candidatos <- unique(c(candidatos, extra))
    }
  }
  
  for (p in candidatos) {
    if (file.exists(p)) return(p)
  }
  
  return(NULL)
}