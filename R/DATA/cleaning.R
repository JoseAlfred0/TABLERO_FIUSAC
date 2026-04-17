# R/data/cleaning.R
parse_fecha_mixta <- function(x) {
  if (is.null(x)) return(as.Date(character()))
  x <- as.character(x)
  x <- trimws(x)
  # Remover comillas internas si existen
  x <- gsub('^"|"$', '', x)
  x <- gsub('"', '', x)
  
  # Vacíos a NA
  x[x == "" | tolower(x) %in% c("na", "null", "nan")] <- NA_character_
  
  # YYYY-MM -> YYYY-MM-01
  es_ym <- grepl("^\\d{4}-\\d{2}$", x)
  x[es_ym] <- paste0(x[es_ym], "-01")
  
  suppressWarnings(as.Date(x))
}

normalizar_datos_historicos <- function(datos, columnas_objetivo) {
  # Limpieza rápida
  datos <- janitor::clean_names(datos)
  
  # Normalización de columnas
  nombres_actuales <- names(datos)
  
  # Mapeo de nombres posibles
  mapa_nombres <- list(
    "id" = "identificador",
    "carnet" = "identificador", 
    "codigo" = "identificador",
    "anio" = "ano",
    "year" = "ano",
    "materia" = "curso",
    "asignatura" = "curso",
    "semestre" = "periodo",
    "tipo_curso" = "modalidad"
  )
  
  # Aplicar cambios solo si existen
  for (nombre_viejo in names(mapa_nombres)) {
    if (nombre_viejo %in% nombres_actuales) {
      data.table::setnames(datos, nombre_viejo, mapa_nombres[[nombre_viejo]], skip_absent = TRUE)
    }
  }
  
  # Asegurar que las columnas objetivo existen
  for (col in columnas_objetivo) {
    if (!col %in% names(datos)) {
      datos[, (col) := NA]
    }
  }
  
  # Conversiones eficientes usando data.table
  if ("identificador" %in% names(datos)) datos[, identificador := as.character(identificador)]
  if ("ano" %in% names(datos)) datos[, ano := as.integer(ano)]
  if ("nota" %in% names(datos)) datos[, nota := as.numeric(nota)]
  if ("curso" %in% names(datos)) datos[, curso := as.character(curso)]
  if ("periodo" %in% names(datos)) datos[, periodo := as.character(periodo)]
  if ("modalidad" %in% names(datos)) datos[, modalidad := as.character(modalidad)]
  
  
  # Eliminar filas de encabezado repetido o ruido (común al integrar múltiples CSV)
  if ("identificador" %in% names(datos)) {
    datos <- datos[!(tolower(trimws(as.character(identificador))) %in% c("identificador", "id", "carnet"))]
  }
  if ("curso" %in% names(datos)) {
    datos <- datos[!(tolower(trimws(as.character(curso))) %in% c("codcurso", "curso"))]
  }
  # Filtrar registros válidos
  datos <- datos[!is.na(identificador) & identificador != ""]
  
  # Eliminar duplicados
  setkey(datos, NULL)
  datos <- unique(datos)
  
  datos
}

normalizar_datos_complementarios <- function(comp) {
  # Normalizar nombres
  nms <- tolower(trimws(names(comp)))
  data.table::setnames(comp, nms)
  
  # Asegurar identificador como texto
  if ("identificador" %in% names(comp)) {
    comp[, identificador := trimws(as.character(identificador))]
  } else {
    log_message("error", "Datos complementarios no contienen la columna 'identificador'.")
    return(NULL)
  }
  
  # Normalizar fechas
  if ("inscrito" %in% names(comp)) comp[, inscrito_date := parse_fecha_mixta(inscrito)]
  if ("cierre" %in% names(comp)) comp[, cierre_date := parse_fecha_mixta(cierre)]
  if ("graduacion" %in% names(comp)) comp[, graduacion_date := parse_fecha_mixta(graduacion)]
  
  # Regla de negocio: si hay graduación y cierre está vacío, usar cierre = graduación (como mínimo coherente)
  if ("graduacion_date" %in% names(comp) && "cierre_date" %in% names(comp)) {
    comp[!is.na(graduacion_date) & is.na(cierre_date), cierre_date := graduacion_date]
  }
  
  # Indexar para búsquedas rápidas por estudiante
  data.table::setkey(comp, identificador)
  
  comp
}