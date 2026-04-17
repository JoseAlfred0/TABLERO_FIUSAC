# R/data/loaders.R
cargar_complementarios_optimizado <- function() {
  archivo_comp <- resolver_ruta_complementarios()
  if (is.null(archivo_comp)) {
    log_message("warning", "Archivo de datos complementarios no encontrado en 'datos'.")
    return(NULL)
  }
  
  log_message("info", paste("Cargando datos complementarios:", archivo_comp))
  
  comp <- data.table::fread(
    archivo_comp,
    sep = ",",
    header = TRUE,
    encoding = "UTF-8",
    showProgress = FALSE,
    na.strings = c("", "NA", "N/A", "NULL", "null")
  )
  
  normalizar_datos_complementarios(comp)
}

cargar_datos_optimizado <- function() {
  log_message("titulo", "CARGA OPTIMIZADA DE DATOS - V7.0")
  
  # Verificar archivos
  archivo_historico <- resolver_ruta_datos()
  
  if (!file.exists(archivo_historico)) {
    stop("Archivo histórico no encontrado: ", archivo_historico)
  }
  
  # Columnas objetivo
  columnas_objetivo <- c("identificador", "ano", "curso", "nombrecurso", "nota", "periodo", "modalidad")
  
  log_message("subtitulo", "Cargando datos con data.table...")
  
  # Leer encabezados para determinar columnas disponibles
  encabezados <- tryCatch(
    data.table::fread(archivo_historico, nrows = 0, encoding = "UTF-8", showProgress = FALSE),
    error = function(e) NULL
  )
  
  columnas_select <- NULL
  
  if (!is.null(encabezados)) {
    orig_names <- names(encabezados)
    clean_names <- janitor::make_clean_names(orig_names)
    
    # Mapeo de nombres posibles
    mapa_nombres <- list(
      "id" = "identificador",
      "carnet" = "identificador",
      "codigo" = "identificador",
      "identificador" = "identificador",
      "anio" = "ano",
      "ano" = "ano",
      "year" = "ano",
      "materia" = "curso",
      "asignatura" = "curso",
      "curso" = "curso",
      "nota" = "nota",
      "calificacion" = "nota",
      "periodo" = "periodo",
      "semestre" = "periodo",
      "modalidad" = "modalidad",
      "tipo_curso" = "modalidad"
    )
    
    mapped <- vapply(clean_names, function(nm) {
      if (!is.null(mapa_nombres[[nm]])) mapa_nombres[[nm]] else nm
    }, character(1))
    
    # Seleccionar columnas existentes
    columnas_select <- vapply(columnas_objetivo, function(target) {
      idx <- which(mapped == target)
      if (length(idx) == 0) NA_character_ else orig_names[idx[1]]
    }, character(1))
    
    if (any(is.na(columnas_select))) {
      columnas_select <- NULL
    }
  }
  
  # Cargar datos
  if (is.null(columnas_select)) {
    datos <- data.table::fread(
      archivo_historico,
      encoding = "UTF-8",
      na.strings = c("", "NA", "N/A", "NULL", "null"),
      showProgress = FALSE
    )
  } else {
    datos <- data.table::fread(
      archivo_historico,
      select = columnas_select,
      encoding = "UTF-8",
      na.strings = c("", "NA", "N/A", "NULL", "null"),
      showProgress = FALSE
    )
  }
  
  datos <- normalizar_datos_historicos(datos, columnas_objetivo)
  
  log_message("info", paste("Registros cargados:", format(nrow(datos), big.mark = ",")))
  log_message("info", paste("Estudiantes únicos:",
                            format(length(unique(datos$identificador)), big.mark = ",")))
  
  return(datos)
}

# Memoización de la carga de datos (evita trabajo repetido)
cargar_datos_memoised <- memoise::memoise(cargar_datos_optimizado)