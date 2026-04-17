log_message <- function(tipo, mensaje) {
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  if (tipo == "titulo") {
    cat(rep("═", 60), "\n", sep = "")
    cat(">>> ", mensaje, "\n", sep = "")
    cat(rep("═", 60), "\n\n", sep = "")
    
  } else if (tipo == "subtitulo") {
    cat(rep("─", 60), "\n", sep = "")
    cat("• ", mensaje, "\n", sep = "")
    cat(rep("─", 60), "\n", sep = "")
    
  } else {
    cat("[", timestamp, "] ", mensaje, "\n", sep = "")
  }
}
