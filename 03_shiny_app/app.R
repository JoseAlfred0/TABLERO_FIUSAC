# ==============================================================================
# TABLERO DE EFICACIA ACADÉMICA - FIUSAC (Versión 7.0 - Producción)
# Arquitectura Modular Optimizada - R 4.5.1
# 
# SECCIONES:
# 1. Análisis Individual: Perfil estudiante, historial académico
# 2. Análisis Institucional: Métricas globales, comparativas
# 3. Series de Tiempo: Evolución, tendencias, cohortes
# 
# OPTIMIZACIONES IMPLEMENTADAS:
# - Carga diferida con caché global
# - Memoización de cálculos repetitivos
# - Data.table para operaciones vectorizadas
# - Manejo robusto de errores con fallbacks
# - Limpieza automática de memoria
# ==============================================================================

source(file.path("..", "R", "core", "logging.R"))
source(file.path("..", "R", "core", "notifications.R"))
source(file.path("..", "R", "core", "startup_checks.R"))
source(file.path("..", "R", "core", "logging.R"))
source(file.path("..", "R", "core", "notifications.R"))
source(file.path("..", "R", "core", "startup_checks.R"))

source(file.path("..", "R", "config", "paths.R"))

source(file.path("..", "R", "data", "cleaning.R"))
source(file.path("..", "R", "data", "loaders.R"))
source(file.path("..", "R", "data", "cache.R"))

source(file.path("..", "R", "modules", "filtros", "ui.R"))
source(file.path("..", "R", "modules", "filtros", "server.R"))

source(file.path("..", "R", "modules", "individual", "ui.R"))
source(file.path("..", "R", "modules", "individual", "server.R"))


source(file.path("..", "R", "modules", "institucional", "ui.R"))
source(file.path("..", "R", "modules", "institucional", "server.R"))

source(file.path("..", "R", "modules", "series", "ui.R"))
source(file.path("..", "R", "modules", "series", "server.R"))


# ------------------------------------------------------------------------------
# FUNCIÓN PARA MENSAJES FORMATEADOS
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# FUNCIÓN UNIFICADA DE NOTIFICACIONES
# ------------------------------------------------------------------------------
notify_user <- function(mensaje, tipo = "default", duracion = 5) {
  tipos_validos <- c("default", "message", "warning", "error")
  if (!tipo %in% tipos_validos) tipo <- "default"
  shiny::showNotification(mensaje, type = tipo, duration = duracion)
}

# ------------------------------------------------------------------------------
# CONFIGURACIÓN OPTIMIZADA Y CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  # Core Shiny
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  
  # Datos eficientes
  library(dplyr)
  library(tidyr)
  library(data.table)  # Más rápido para grandes datasets
  library(readr)
  library(janitor)
  
  # Visualización
  library(plotly)
  library(ggplot2)
  library(DT)
  
  # Utilidades
  library(lubridate)
  library(stringr)
  library(scales)
  library(purrr)
  
  # Optimización
  library(memoise)     # Para memoización
  library(digest)      # Para generar hashes
  
  # Exportación
  library(writexl)     # Para exportar a Excel
})


# ------------------------------------------------------------------------------
# BOOTSTRAP DE RUTAS (PROYECTO ROBUSTO)
# Ejecuta la app desde 03_shiny_app/ o usando run_app.R desde la raíz.
# ------------------------------------------------------------------------------
resolver_ruta_datos <- function() {
  # Prioridad 1: proyecto robusto
  p1 <- file.path("..", "01_data", "datos_2001_2024.csv")
  if (file.exists(p1)) return(p1)

  # Compatibilidad 2: estructura anterior
  p2 <- file.path("datos", "datos_2001_2024.csv")
  if (file.exists(p2)) return(p2)

  # Mensaje claro
  return(p1)
}


# ------------------------------------------------------------------------------
# RUTA Y CARGA DE DATOS COMPLEMENTARIOS (INSCRIPCIÓN / CIERRE / GRADUACIÓN)
# ------------------------------------------------------------------------------
resolver_ruta_complementarios <- function() {
  # Ruta esperada dentro de la app (Shiny Server): 03_shiny_app/datos/
  candidatos <- c(
    file.path("datos", "datos_complementarios_limpios.csv"),
    file.path("datos", "datos_complementarios.csv"),
    file.path("datos", "datos_complementarios - copia.csv")
  )

  # Buscar cualquier variante que cumpla el patrón
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

# Convierte fechas mixtas: acepta 'YYYY-MM-DD' o 'YYYY-MM' (lo convierte a 'YYYY-MM-01')
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

cargar_complementarios_optimizado <- function() {
  archivo_comp <- resolver_ruta_complementarios()
  if (is.null(archivo_comp)) {
    log_message("warning", "Archivo de datos complementarios no encontrado en 'datos/'.")
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

# ------------------------------------------------------------------------------
# SISTEMA DE CACHÉ GLOBAL
# ------------------------------------------------------------------------------

# Caché global de datos para evitar recargar el dataset por sesión
.datos_cache_env <- new.env(parent = emptyenv())

# ------------------------------------------------------------------------------
# CARGADOR DE DATOS OPTIMIZADO
# ------------------------------------------------------------------------------
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
  
  log_message("info", paste("Registros cargados:", format(nrow(datos), big.mark = ",")))
  log_message("info", paste("Estudiantes únicos:", 
                            format(length(unique(datos$identificador)), big.mark = ",")))
  
  return(datos)
}


# Memoización de la carga de datos (evita trabajo repetido)
cargar_datos_memoised <- memoise::memoise(cargar_datos_optimizado)

# ------------------------------------------------------------------------------
# MÓDULOS DE LA APLICACIÓN
# ------------------------------------------------------------------------------

# Módulo: Filtros inteligentes
mod_filtros_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinydashboard::box(
      title = "🎯 Filtros Inteligentes",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      
      shiny::fluidRow(
        shiny::column(4,
                      shiny::selectizeInput(
                        ns("seccion_activa"),
                        "Sección de Análisis:",
                        choices = c(
                          "Análisis Individual" = "individual",
                          "Análisis Institucional" = "institucional",
                          "Series de Tiempo" = "series"
                        ),
                        selected = "individual"
                      )
        ),
        shiny::column(8,
                      shiny::uiOutput(ns("filtros_contextuales"))
        )
      ),
      
      shiny::hr(),
      
      # Filtros comunes a todas las secciones
      shiny::fluidRow(
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_rango_anos"),
                        "Rango de Años:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Seleccione años...',
                          plugins = list('remove_button')
                        )
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_modalidad"),
                        "Modalidad:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Todas las modalidades...',
                          plugins = list('remove_button')
                        )
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_periodo"),
                        "Período:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Todos los períodos...',
                          plugins = list('remove_button')
                        )
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_curso_grupo"),
                        "Curso (grupo):",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Todos los cursos...',
                          maxOptions = 100,
                          plugins = list('remove_button')
                        )
                      )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(6,
                      shiny::actionButton(ns("aplicar_filtros"), "Aplicar Filtros", 
                                          icon = shiny::icon("filter"), 
                                          class = "btn-primary btn-block",
                                          style = "margin-top: 10px;")
        ),
        shiny::column(6,
                      shiny::actionButton(ns("limpiar_filtros"), "Limpiar Todo", 
                                          icon = shiny::icon("broom"), 
                                          class = "btn-default btn-block",
                                          style = "margin-top: 10px;")
        )
      )
    )
  )
}

mod_filtros_server <- function(id, datos_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valores iniciales de los filtros
    filtros_aplicados <- shiny::reactiveValues(
      seccion = "individual",
      rango_anos = NULL,
      modalidad = NULL,
      periodo = NULL,
      curso = NULL,
      estudiante = NULL,
      id_manual = NULL
    )
    
    # Mantener el valor de sección en el estado aplicado (sin mutar dentro de renderUI)
    shiny::observeEvent(input$seccion_activa, {
      filtros_aplicados$seccion <- input$seccion_activa
    }, ignoreInit = FALSE)
    
    # Filtros contextuales según sección
    output$filtros_contextuales <- shiny::renderUI({
      shiny::req(datos_reactive())
      
      seccion <- input$seccion_activa
      if (seccion == "individual") {
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6,
                          shiny::selectizeInput(
                            ns("select_estudiante"),
                            "Seleccionar Estudiante:",
                            choices = NULL,
                            selected = "",
                            options = list(
                              placeholder = 'Seleccione un estudiante...',
                              maxOptions = 100
                            )
                          )
            ),
            shiny::column(6,
                          shiny::textInput(ns("id_manual"), "ID Manual:", 
                                           placeholder = "Ej: 201234567")
            )
          )
        )
      } else if (seccion == "institucional") {
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6,
                          shiny::sliderInput(
                            ns("rango_notas"),
                            "Rango de Notas:",
                            min = 0, max = 100,
                            value = c(0, 100)
                          )
            ),
            shiny::column(6,
                          shiny::checkboxGroupInput(
                            ns("tipo_analisis"),
                            "Tipo de Análisis:",
                            choices = c("Comparativa cursos" = "cursos",
                                        "Modalidades" = "modalidades",
                                        "Tendencias" = "tendencias"),
                            selected = "cursos"
                          )
            )
          )
        )
      } else { # series de tiempo
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6,
                          shiny::selectInput(
                            ns("tipo_serie"),
                            "Tipo de Serie:",
                            choices = c("Promedio anual" = "promedio",
                                        "Tasa aprobación" = "aprobacion",
                                        "Distribución" = "distribucion")
                          )
            ),
            shiny::column(6,
                          shiny::numericInput(
                            ns("ventana_suavizado"),
                            "Suavizado (años):",
                            value = 1,
                            min = 1,
                            max = 5,
                            step = 1
                          )
            )
          )
        )
      }
    })
    
    # Actualizar filtros con datos disponibles
    shiny::observe({
      shiny::req(datos_reactive())
      
      datos <- datos_reactive()
      
      tryCatch({
        # Evitar bucles reactivos al actualizar selectizeInputs programáticamente
        shiny::freezeReactiveValue(input, "filtro_rango_anos")
        shiny::freezeReactiveValue(input, "filtro_modalidad")
        shiny::freezeReactiveValue(input, "filtro_periodo")
        shiny::freezeReactiveValue(input, "filtro_curso_grupo")
        # Actualizar años disponibles
        anos <- integer(0)
        if ("ano" %in% names(datos)) {
          anos_raw <- datos$ano
          anos_raw <- suppressWarnings(as.integer(anos_raw))
          anos_raw <- anos_raw[!is.na(anos_raw)]
          anos <- sort(unique(anos_raw))
        }
        if (length(anos) == 0) anos <- integer(0)
        shiny::updateSelectizeInput(
          session, "filtro_rango_anos",
          choices = anos,
          selected = if (is.null(filtros_aplicados$rango_anos)) character(0) else filtros_aplicados$rango_anos
        )
        
        # Actualizar modalidades
        modalidades <- if ("modalidad" %in% names(datos)) sort(unique(datos$modalidad)) else character(0)
        modalidades <- modalidades[!is.na(modalidades) & modalidades != ""]
        if (length(modalidades) == 0) modalidades <- "No especificado"
        shiny::updateSelectizeInput(
          session, "filtro_modalidad",
          choices = modalidades,
          selected = if (is.null(filtros_aplicados$modalidad)) character(0) else filtros_aplicados$modalidad
        )
        
        # Actualizar períodos
        periodos <- if ("periodo" %in% names(datos)) sort(unique(datos$periodo)) else character(0)
        periodos <- periodos[!is.na(periodos) & periodos != ""]
        if (length(periodos) == 0) periodos <- "No especificado"
        shiny::updateSelectizeInput(
          session, "filtro_periodo",
          choices = periodos,
          selected = if (is.null(filtros_aplicados$periodo)) character(0) else filtros_aplicados$periodo
        )
        
        # Actualizar cursos (solo top 100 para eficiencia)
        cursos <- if ("curso" %in% names(datos)) sort(unique(datos$curso)) else character(0)
        cursos <- cursos[!is.na(cursos) & cursos != ""]
        cursos <- head(cursos, 100)
        if (length(cursos) == 0) cursos <- "No especificado"
        shiny::updateSelectizeInput(
          session, "filtro_curso_grupo",
          choices = cursos,
          selected = if (is.null(filtros_aplicados$curso)) character(0) else filtros_aplicados$curso
        )
        
        # Actualizar estudiantes (Selectize server-side para no enviar miles de opciones al navegador)
        if (isTRUE(identical(input$seccion_activa, "individual"))) {
          ids <- character(0)
          if ("identificador" %in% names(datos)) {
            ids <- unique(datos$identificador)
            ids <- ids[!is.na(ids) & ids != ""]
            ids <- sort(ids)
          }
          shiny::updateSelectizeInput(
            session, "select_estudiante",
            choices = c("", ids),
            selected = if (is.null(filtros_aplicados$estudiante) || filtros_aplicados$estudiante == "") "" else filtros_aplicados$estudiante,
            server = TRUE
          )
        }
        
      }, error = function(e) {
        notify_user(paste('Error al actualizar filtros:', e$message), tipo = 'error', duracion = 8)
      })
      
    })
    # Nota: La selección de estudiante/ID manual se confirma únicamente al presionar "Aplicar Filtros"
    # Aplicar filtros cuando se presiona el botón
    shiny::observeEvent(input$aplicar_filtros, {
      # Actualizar valores de filtros
      filtros_aplicados$rango_anos <- input$filtro_rango_anos
      filtros_aplicados$modalidad <- input$filtro_modalidad
      filtros_aplicados$periodo <- input$filtro_periodo
      filtros_aplicados$curso <- input$filtro_curso_grupo
      
      
      # Confirmar selección de estudiante/ID manual solo al aplicar (evita inestabilidad por reactividad inmediata)
      filtros_aplicados$estudiante <- if (!is.null(input$select_estudiante)) input$select_estudiante else NULL
      filtros_aplicados$id_manual  <- if (!is.null(input$id_manual)) input$id_manual else NULL
      # Mostrar notificación
      # Filtros inteligentes (solo análisis individual):
      # Al aplicar por primera vez con un estudiante, se rellenan y acotan las opciones
      # de años, modalidad, período y curso con la información REAL del estudiante.
      if (isTRUE(identical(filtros_aplicados$seccion, "individual"))) {
        estudiante_sel <- filtros_aplicados$estudiante
        id_manual_sel  <- filtros_aplicados$id_manual
        id_final <- if (!is.null(estudiante_sel) && estudiante_sel != "") estudiante_sel else id_manual_sel

        if (!is.null(id_final) && id_final != "") {
          datos_local <- tryCatch(datos_reactive(), error = function(e) NULL)
          if (!is.null(datos_local) && nrow(datos_local) > 0 && "identificador" %in% names(datos_local)) {
            if (!data.table::is.data.table(datos_local)) datos_local <- data.table::as.data.table(datos_local)
            de <- datos_local[identificador == id_final]
            if (nrow(de) > 0) {
              anos_est <- sort(unique(de$ano[!is.na(de$ano)]))
              mod_est  <- sort(unique(de$modalidad[!is.na(de$modalidad) & trimws(as.character(de$modalidad)) != ""]))
              per_est  <- sort(unique(de$periodo[!is.na(de$periodo) & trimws(as.character(de$periodo)) != ""]))
              cur_est  <- sort(unique(de$curso[!is.na(de$curso) & trimws(as.character(de$curso)) != ""]))

              # Congelar reactividad para evitar loops
              shiny::freezeReactiveValue(input, "filtro_rango_anos")
              shiny::freezeReactiveValue(input, "filtro_modalidad")
              shiny::freezeReactiveValue(input, "filtro_periodo")
              shiny::freezeReactiveValue(input, "filtro_curso_grupo")

              updateSelectizeInput(session, "filtro_rango_anos",
                                   choices = as.character(anos_est),
                                   selected = as.character(anos_est),
                                   server = TRUE)
              updateSelectizeInput(session, "filtro_modalidad",
                                   choices = mod_est,
                                   selected = mod_est,
                                   server = TRUE)
              updateSelectizeInput(session, "filtro_periodo",
                                   choices = per_est,
                                   selected = per_est,
                                   server = TRUE)
              updateSelectizeInput(session, "filtro_curso_grupo",
                                   choices = cur_est,
                                   selected = cur_est,
                                   server = TRUE)

              # Persistir en valores aplicados
              filtros_aplicados$rango_anos <- as.character(anos_est)
              filtros_aplicados$modalidad <- mod_est
              filtros_aplicados$periodo <- per_est
              filtros_aplicados$curso <- cur_est
            }
          }
        }
      }
      notify_user("Filtros aplicados correctamente", 
                  tipo = "message",
                  duracion = 3)
    })
    
    # Limpiar filtros
    shiny::observeEvent(input$limpiar_filtros, {
      # Limpiar todos los filtros
      shiny::updateSelectizeInput(session, "filtro_rango_anos", selected = character(0))
      shiny::updateSelectizeInput(session, "filtro_modalidad", selected = character(0))
      shiny::updateSelectizeInput(session, "filtro_periodo", selected = character(0))
      shiny::updateSelectizeInput(session, "filtro_curso_grupo", selected = character(0))
      shiny::updateSelectizeInput(session, "select_estudiante", selected = "")
      shiny::updateTextInput(session, "id_manual", value = "")
      
      # Reiniciar valores
      filtros_aplicados$rango_anos <- NULL
      filtros_aplicados$modalidad <- NULL
      filtros_aplicados$periodo <- NULL
      filtros_aplicados$curso <- NULL
      filtros_aplicados$estudiante <- NULL
      filtros_aplicados$id_manual <- NULL
      
      notify_user("Filtros limpiados", 
                  tipo = "default", 
                  duracion = 3)
    })
    
    # Retornar filtros aplicados como lista reactiva
    shiny::reactive({
      list(
        seccion = filtros_aplicados$seccion,
        rango_anos = filtros_aplicados$rango_anos,
        modalidad = filtros_aplicados$modalidad,
        periodo = filtros_aplicados$periodo,
        curso = filtros_aplicados$curso,
        estudiante = filtros_aplicados$estudiante,
        id_manual = filtros_aplicados$id_manual
      )
    })
  })
}

# Módulo: Análisis Individual (optimizado)
mod_individual_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h2("👤 Análisis Individual - Perfil del Estudiante"),
    
    # Panel de validación
    shiny::uiOutput(ns("panel_validacion")),
    
    # KPIs del estudiante
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("kpi_estado"), width = 3),
      shinydashboard::valueBoxOutput(ns("kpi_promedio"), width = 3),
      shinydashboard::valueBoxOutput(ns("kpi_aprobacion"), width = 3),
      shinydashboard::valueBoxOutput(ns("kpi_trayectoria"), width = 3)
    ),
    
    # Información detallada
    shiny::fluidRow(
      shinydashboard::box(
        title = "📊 Rendimiento Académico",
        status = "primary",
        solidHeader = TRUE,
        width = 8,
        collapsible = TRUE,
        shiny::tabsetPanel(
          shiny::tabPanel("Notas por Curso", 
                          plotly::plotlyOutput(ns("grafico_cursos")) %>% 
                            shinycssloaders::withSpinner(color = "#3498db")),
          shiny::tabPanel("Evolución Temporal", 
                          plotly::plotlyOutput(ns("grafico_evolucion")) %>% 
                            shinycssloaders::withSpinner(color = "#3498db")),
          shiny::tabPanel("Distribución", 
                          plotly::plotlyOutput(ns("grafico_distribucion")) %>% 
                            shinycssloaders::withSpinner(color = "#3498db"))
        )
      ),
      
      shinydashboard::box(
        title = "📋 Información del Estudiante",
        status = "info",
        solidHeader = TRUE,
        width = 4,
        collapsible = TRUE,
        shiny::uiOutput(ns("info_estudiante")),
        shiny::hr(),
        shiny::h5("Acciones:"),
        shiny::downloadButton(ns("descargar_reporte"), "Descargar Reporte", 
                              class = "btn-primary btn-block",
                              style = "margin-bottom: 10px;"),
        shiny::actionButton(ns("comparar_estudiante"), "Comparar con promedio", 
                            class = "btn-default btn-block")
      )
    ),
    
    # Historial completo
    shiny::fluidRow(
      shinydashboard::box(
        title = "📚 Historial Académico Completo",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        DT::DTOutput(ns("tabla_historial")) %>% 
          shinycssloaders::withSpinner(color = "#3498db")
      )
    )
  )
}

mod_individual_server <- function(id, datos_filtrados, datos_complementarios = NULL, filtros_modulo_id = "filtros_individual") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Datos del estudiante seleccionado (optimizado con data.table)
    datos_estudiante <- shiny::reactive({
      shiny::req(datos_filtrados())
      datos <- datos_filtrados()
      
      # Si no hay datos, retornar data.table vacío
      if (is.null(datos) || nrow(datos) == 0) {
        return(data.table::data.table())
      }
      
      # Normalizar a data.table para evitar mezclas y acelerar operaciones
      if (!data.table::is.data.table(datos)) {
        datos <- data.table::as.data.table(datos)
      }
      
      # Validación: análisis individual requiere un único estudiante
      if (!("identificador" %in% names(datos)) || data.table::uniqueN(datos$identificador) != 1) {
        return(data.table::data.table())
      }
      
      return(datos)
    })
    
    # Panel de validación
    output$panel_validacion <- shiny::renderUI({
      datos <- datos_estudiante()
      
      if (nrow(datos) == 0) {
        shinydashboard::box(
          title = "Seleccione un Estudiante",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          shiny::div(
            class = "alert alert-warning",
            shiny::h4(shiny::icon("user-graduate"), " Instrucciones:"),
            shiny::tags$ol(
              shiny::tags$li("Seleccione un estudiante de la lista en la barra lateral"),
              shiny::tags$li("O ingrese el ID manualmente"),
              shiny::tags$li("Haga clic en 'Aplicar Filtros'"),
              shiny::tags$li("La información se cargará automáticamente")
            ),
            shiny::hr(),
            shiny::h5("Estudiantes disponibles (primeros 20):"),
            shiny::uiOutput(ns("lista_estudiantes"))
          )
        )
      }
    })
    
    # Lista de estudiantes (optimizada)
    output$lista_estudiantes <- shiny::renderUI({
      shiny::req(datos_filtrados())
      
      # Obtener estudiantes únicos de manera eficiente
      if (is.data.table(datos_filtrados())) {
        estudiantes <- unique(datos_filtrados()$identificador)
      } else {
        estudiantes <- unique(datos_filtrados()$identificador)
      }
      
      estudiantes <- head(sort(estudiantes[!is.na(estudiantes)]), 20)
      
      shiny::tagList(
        lapply(estudiantes, function(est) {
          shiny::tags$button(
            class = "btn btn-default btn-sm",
            style = "margin: 2px; padding: 5px 10px;",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              paste0(filtros_modulo_id, "-select_estudiante"), est),
            est
          )
        })
      )
    })
    
    # KPIs optimizados
    output$kpi_estado <- shinydashboard::renderValueBox({
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        return(shinydashboard::valueBox("No seleccionado", "Estado académico",
                                        icon = shiny::icon("user-graduate"), color = "red"))
      }

      estudiante_id <- tryCatch(unique(datos$identificador)[1], error = function(e) NA_character_)

      # Determinar estado académico según datos complementarios (prioridad)
      estado <- "En proceso"
      if (!is.null(datos_complementarios)) {
        comp <- tryCatch(datos_complementarios(), error = function(e) NULL)
        if (!is.null(comp) && nrow(comp) > 0 && "identificador" %in% names(comp) && !is.na(estudiante_id) && estudiante_id != "") {
          comp_df <- comp[comp$identificador == estudiante_id, , drop = FALSE]

          if (nrow(comp_df) > 0) {
            # Normalizar nombres esperados
            if (!("graduacion" %in% names(comp_df)) && ("fecha_graduacion" %in% names(comp_df))) comp_df$graduacion <- comp_df$fecha_graduacion
            if (!("cierre" %in% names(comp_df)) && ("fecha_cierre" %in% names(comp_df))) comp_df$cierre <- comp_df$fecha_cierre

            # Parseo flexible de fechas
            fecha_mixta <- function(x) {
              x <- as.character(x)
              x <- trimws(x)
              x[x == ""] <- NA_character_
              out <- suppressWarnings(as.Date(x))
              if (all(is.na(out))) {
                out <- suppressWarnings(lubridate::ymd(x))
              }
              if (all(is.na(out))) {
                out <- suppressWarnings(lubridate::dmy(x))
              }
              if (all(is.na(out))) {
                out <- suppressWarnings(lubridate::mdy(x))
              }
              out
            }

            graduacion_v <- if ("graduacion" %in% names(comp_df)) fecha_mixta(comp_df$graduacion) else as.Date(rep(NA, nrow(comp_df)))
            cierre_v <- if ("cierre" %in% names(comp_df)) fecha_mixta(comp_df$cierre) else as.Date(rep(NA, nrow(comp_df)))

            if (any(!is.na(graduacion_v))) {
              estado <- "Graduado"
            } else if (any(!is.na(cierre_v))) {
              estado <- "Cierre"
            } else {
              estado <- "En proceso"
            }
          }
        }
      }

      color <- if (estado == "Graduado") "green" else if (estado == "Cierre") "yellow" else "aqua"

      shinydashboard::valueBox(estado, "Estado académico",
                               icon = shiny::icon("user-graduate"), color = color)
    })

    output$kpi_promedio <- shinydashboard::renderValueBox({
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        return(shinydashboard::valueBox("N/A", "Promedio (aprobados)",
                                        icon = shiny::icon("calculator"), color = "red"))
      }

      # Normalizar a data.table
      if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

      # Resumen por curso (usa la mejor nota por curso para evitar duplicidad por repitencia)
      resumen_curso <- datos[!is.na(curso) & trimws(as.character(curso)) != "" & !is.na(nota),
                             .(nota_max = suppressWarnings(max(nota, na.rm = TRUE))),
                             by = curso]

      notas_aprobadas <- resumen_curso[!is.na(nota_max) & nota_max >= 61, nota_max]

      if (length(notas_aprobadas) == 0) {
        return(shinydashboard::valueBox("N/A", "Promedio (aprobados)",
                                        icon = shiny::icon("calculator"), color = "red"))
      }

      promedio <- round(mean(notas_aprobadas, na.rm = TRUE), 1)
      color <- if (promedio >= 80) "green" else if (promedio >= 70) "yellow" else "aqua"

      shinydashboard::valueBox(promedio, "Promedio (aprobados)",
                               icon = shiny::icon("calculator"), color = color)
    })

    output$kpi_aprobacion <- shinydashboard::renderValueBox({
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        return(shinydashboard::valueBox("N/A", "Cursos aprobados",
                                        icon = shiny::icon("check-circle"), color = "red"))
      }

      if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

      resumen_curso <- datos[!is.na(curso) & trimws(as.character(curso)) != "" & !is.na(nota),
                             .(nota_max = suppressWarnings(max(nota, na.rm = TRUE))),
                             by = curso]

      cursos_aprobados <- sum(resumen_curso$nota_max >= 61, na.rm = TRUE)

      shinydashboard::valueBox(cursos_aprobados, "Cursos aprobados",
                               icon = shiny::icon("check-circle"), color = "green")
    })

    output$kpi_trayectoria <- shinydashboard::renderValueBox({
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        return(shinydashboard::valueBox("N/A", "Cursos tomados",
                                        icon = shiny::icon("book"), color = "red"))
      }

      if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

      cursos_tomados <- data.table::uniqueN(datos[!is.na(curso) & trimws(as.character(curso)) != "", curso])

      shinydashboard::valueBox(cursos_tomados, "Cursos tomados",
                               icon = shiny::icon("book"), color = "aqua")
    })
output$grafico_cursos <- plotly::renderPlotly({
  datos <- datos_estudiante()
  if (nrow(datos) == 0 || all(is.na(datos$nota))) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = "No hay datos disponibles"))
  }

  # Normalizar a data.table
  if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

  # Etiqueta de curso (prioriza nombrecurso si existe)
  if ("nombrecurso" %in% names(datos)) {
    datos[, curso_lbl := ifelse(!is.na(nombrecurso) & trimws(as.character(nombrecurso)) != "",
                               trimws(as.character(nombrecurso)),
                               trimws(as.character(curso)))]
  } else {
    datos[, curso_lbl := trimws(as.character(curso))]
  }

  # Para cada curso:
  # - nota = mejor nota histórica (max)
  # - ano_primero = primer año en que se llevó el curso (min)
  # - ano_ultimo  = último año en que se llevó el curso (max)
  datos_grafico <- datos[!is.na(nota) & !is.na(curso_lbl) & curso_lbl != "" & !is.na(ano),
                         .(nota = suppressWarnings(max(nota, na.rm = TRUE)),
                           ano_primero = suppressWarnings(min(ano, na.rm = TRUE)),
                           ano_ultimo  = suppressWarnings(max(ano, na.rm = TRUE))),
                         by = curso_lbl]

  if (nrow(datos_grafico) == 0) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = "No hay datos disponibles"))
  }

  # Orden cronológico: por el primer año en que se cursó (y desempates)
  data.table::setorder(datos_grafico, ano_primero, ano_ultimo, curso_lbl)

  # Factor para respetar el orden cronológico en el eje X
  datos_grafico[, curso_lbl_f := factor(curso_lbl, levels = curso_lbl)]

  plotly::plot_ly(
    datos_grafico,
    x = ~curso_lbl_f,
    y = ~nota,
    type = "bar",
    marker = list(color = ~ifelse(nota >= 61, "#27ae60", "#e74c3c")),
    text = ~paste0(
      "Curso: ", curso_lbl,
      "<br>Mejor nota: ", nota,
      "<br>Primer año: ", ano_primero,
      "<br>Último año: ", ano_ultimo
    ),
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = "Rendimiento por Curso (orden cronológico)",
      xaxis = list(
        title = "",
        tickangle = 45,
        categoryorder = "array",
        categoryarray = levels(datos_grafico$curso_lbl_f)
      ),
      yaxis = list(title = "Nota", range = c(0, 100)),
      showlegend = FALSE
    )
})
output$grafico_evolucion <- plotly::renderPlotly({
  datos <- datos_estudiante()
  if (nrow(datos) == 0) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = "No hay datos disponibles"))
  }

  if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

  # Etiqueta de curso (para consolidar intentos del mismo curso en el año)
  if ("nombrecurso" %in% names(datos)) {
    datos[, curso_lbl := ifelse(!is.na(nombrecurso) & trimws(as.character(nombrecurso)) != "",
                               trimws(as.character(nombrecurso)),
                               trimws(as.character(curso)))]
  } else {
    datos[, curso_lbl := trimws(as.character(curso))]
  }

  # Consolidar por (año, curso): usar mejor nota del curso en ese año
  datos_anio_curso <- datos[!is.na(ano) & !is.na(nota) & !is.na(curso_lbl) & curso_lbl != "",
                            .(nota_max = suppressWarnings(max(nota, na.rm = TRUE))),
                            by = .(ano, curso_lbl)]

  if (nrow(datos_anio_curso) == 0) return(plotly::plotly_empty())

  # Totales por año (cursos tomados)
  totales <- datos_anio_curso[, .(cursos_tomados = .N), by = ano]

  # Solo aprobados (nota >= 61) para el promedio anual
  aprob <- datos_anio_curso[nota_max >= 61,
                            .(promedio = round(mean(nota_max, na.rm = TRUE), 2),
                              cursos_aprobados = .N),
                            by = ano]

  datos_df <- merge(totales, aprob, by = "ano", all.x = TRUE)
  datos_df[is.na(cursos_aprobados), cursos_aprobados := 0L]

  # Tasa de aprobación por año (sobre cursos tomados ese año)
  datos_df[, tasa_aprobacion := ifelse(
    cursos_tomados > 0,
    round((cursos_aprobados / cursos_tomados) * 100, 2),
    NA_real_
  )]

  data.table::setorder(datos_df, ano)

  plotly::plot_ly(
    datos_df,
    x = ~ano,
    y = ~promedio,
    type = "scatter",
    mode = "lines+markers",
    text = ~paste0(
      "Año: ", ano,
      "<br>Promedio (solo aprobados): ", ifelse(is.na(promedio), "N/A", promedio),
      "<br>Cursos aprobados: ", cursos_aprobados,
      "<br>Cursos tomados: ", cursos_tomados,
      "<br>Tasa aprobación: ", ifelse(is.na(tasa_aprobacion), "N/A", paste0(tasa_aprobacion, "%"))
    ),
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = "Evolución Temporal del Promedio (solo cursos aprobados)",
      xaxis = list(title = "Año"),
      yaxis = list(title = "Promedio", range = c(0, 100))
    )
})
    output$grafico_distribucion <- plotly::renderPlotly({
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = "No hay datos disponibles"))
      }

      if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

      notas <- datos[!is.na(nota), nota]
      if (length(notas) == 0) return(plotly::plotly_empty())

      # Histograma en plotly
      plotly::plot_ly(x = notas, type = "histogram", nbinsx = 20, opacity = 0.75) %>%
        plotly::layout(
          title = "Distribución de Notas",
          xaxis = list(title = "Nota"),
          yaxis = list(title = "Frecuencia"),
          shapes = list(
            list(type = "line",
                 x0 = 61, x1 = 61,
                 y0 = 0, y1 = 1,
                 xref = "x", yref = "paper",
                 line = list(color = "red", dash = "dash"))
          )
        )
    })

    output$info_estudiante <- shiny::renderUI({
      datos <- tryCatch(datos_filtrados(), error = function(e) NULL)
      if (is.null(datos) || nrow(datos) == 0) {
        return(shiny::div("No hay datos del estudiante para mostrar."))
      }

	    # Inicializaciones defensivas (evita 'objeto no encontrado')
	    fecha_inscripcion_txt <- "No disponible"
	    fecha_cierre_txt <- "No disponible"
	    fecha_graduacion_txt <- "No disponible"
	    estado_academico_txt <- "En proceso"
	    carreras_txt <- "No disponible"
	    tabla_carreras <- NULL

      # Datos base del historial (siempre existen)
      estudiante_id <- tryCatch(unique(datos$identificador)[1], error = function(e) NA_character_)
      if (is.na(estudiante_id) || estudiante_id == "") {
        return(shiny::div("Identificador de estudiante no disponible."))
      }

      # Recuperar datos complementarios de forma segura (puede no existir el archivo)
      comp <- NULL
      if (!is.null(datos_complementarios)) {
        comp <- tryCatch(datos_complementarios(), error = function(e) NULL)
      }
      if (!is.null(comp) && nrow(comp) > 0 && !data.table::is.data.table(comp)) {
        comp <- data.table::as.data.table(comp)
      }
      if (data.table::is.data.table(comp) && "identificador" %in% names(comp) && !data.table::haskey(comp)) {
        data.table::setkey(comp, identificador)
      }

      if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)

      # Distinción clave solicitada:
      # - Cursos tomados: cursos únicos (por código de curso)
      # - Cursos aprobados: cursos con nota >= 61 (considerando la mejor nota por curso)
      resumen_curso <- datos[!is.na(curso) & trimws(as.character(curso)) != "" & !is.na(nota),
                             .(nota_max = suppressWarnings(max(nota, na.rm = TRUE))),
                             by = curso]

      cursos_tomados <- nrow(resumen_curso)
      cursos_aprobados <- sum(resumen_curso$nota_max >= 61, na.rm = TRUE)
      tasa_aprobacion <- if (cursos_tomados > 0) round((cursos_aprobados / cursos_tomados) * 100, 2) else NA_real_
      promedio_aprobados <- if (cursos_aprobados > 0) round(mean(resumen_curso$nota_max[resumen_curso$nota_max >= 61], na.rm = TRUE), 2) else NA_real_

      años_range <- tryCatch({
        paste(min(datos$ano, na.rm = TRUE), "-", max(datos$ano, na.rm = TRUE))
      }, error = function(e) "No disponible")
      modalidades <- tryCatch({
        paste(sort(unique(na.omit(datos$modalidad))), collapse = ", ")
      }, error = function(e) "No disponible")
      periodos <- tryCatch({
        paste(sort(unique(na.omit(datos$periodo))), collapse = ", ")
      }, error = function(e) "No disponible")
      # NOTA: Un estudiante puede tener varias carreras simultáneas.
      # Por lo tanto, si existen múltiples filas para el mismo identificador,
      # se calcula un RESUMEN GLOBAL y además se muestra un DETALLE POR CARRERA.
      if (!is.null(comp) && nrow(comp) > 0) {
        comp_row <- tryCatch({
          if (data.table::is.data.table(comp) && "identificador" %in% names(comp)) {
            comp[data.table::J(estudiante_id)]
          } else {
            comp[comp$identificador == estudiante_id, , drop = FALSE]
          }
        }, error = function(e) NULL)

        if (!is.null(comp_row) && nrow(comp_row) > 0) {
          comp_df <- as.data.frame(comp_row, stringsAsFactors = FALSE)

          # Extraer fechas (preferir columnas *_date si existen)
          inscrito_v <- if ("inscrito_date" %in% names(comp_df)) comp_df$inscrito_date else if ("inscrito" %in% names(comp_df)) parse_fecha_mixta(comp_df$inscrito) else as.Date(rep(NA, nrow(comp_df)))
          cierre_v <- if ("cierre_date" %in% names(comp_df)) comp_df$cierre_date else if ("cierre" %in% names(comp_df)) parse_fecha_mixta(comp_df$cierre) else as.Date(rep(NA, nrow(comp_df)))
          graduacion_v <- if ("graduacion_date" %in% names(comp_df)) comp_df$graduacion_date else if ("graduacion" %in% names(comp_df)) parse_fecha_mixta(comp_df$graduacion) else as.Date(rep(NA, nrow(comp_df)))

          # Resumen global (min inscripción, max cierre y max graduación)
          if (any(!is.na(inscrito_v))) {
            fecha_inscripcion_txt <- format(min(inscrito_v, na.rm = TRUE), "%Y-%m")
          }
          if (any(!is.na(cierre_v))) {
            fecha_cierre_txt <- format(max(cierre_v, na.rm = TRUE), "%Y-%m")
          }
          if (any(!is.na(graduacion_v))) {
            fecha_graduacion_txt <- format(max(graduacion_v, na.rm = TRUE), "%Y-%m")
          }

          # Estado global
          if (any(!is.na(graduacion_v))) {
            estado_academico_txt <- "Graduado"
            if (fecha_cierre_txt == "No disponible") fecha_cierre_txt <- fecha_graduacion_txt
          } else if (any(!is.na(cierre_v))) {
            estado_academico_txt <- "Cierre"
          } else {
            estado_academico_txt <- "En proceso"
          }

          # Carreras (lista)
          if ("carrera" %in% names(comp_df)) {
            carreras_u <- unique(comp_df$carrera)
            carreras_u <- carreras_u[!is.na(carreras_u) & trimws(carreras_u) != ""]
            if (length(carreras_u) > 0) carreras_txt <- paste(carreras_u, collapse = " | ")
          }

          # Detalle por carrera (todas las filas del estudiante)
          det <- data.frame(
            carrera = if ("carrera" %in% names(comp_df)) comp_df$carrera else NA_character_,
            tipo_pensum = if ("tipo_pensum" %in% names(comp_df)) comp_df$tipo_pensum else NA_character_,
            inscrito = ifelse(is.na(inscrito_v), "", format(inscrito_v, "%Y-%m")),
            cierre = ifelse(is.na(cierre_v), "", format(cierre_v, "%Y-%m")),
            graduacion = ifelse(is.na(graduacion_v), "", format(graduacion_v, "%Y-%m")),
            stringsAsFactors = FALSE
          )

          det$estado <- ifelse(det$graduacion != "", "Graduado",
                               ifelse(det$cierre != "", "Cierre", "En proceso"))

          # Quitar filas totalmente vacías (si existieran)
          det <- det[rowSums(det != "" & !is.na(det)) > 0, , drop = FALSE]

          if (nrow(det) > 0) {
            # Render en tabla HTML simple (no requiere outputs adicionales)
            filas <- lapply(seq_len(nrow(det)), function(i) {
              shiny::tags$tr(
                shiny::tags$td(ifelse(is.na(det$carrera[i]) || det$carrera[i] == "", "No especificado", det$carrera[i])),
                shiny::tags$td(ifelse(is.na(det$tipo_pensum[i]) || det$tipo_pensum[i] == "", "No especificado", det$tipo_pensum[i])),
                shiny::tags$td(det$inscrito[i]),
                shiny::tags$td(det$cierre[i]),
                shiny::tags$td(det$graduacion[i]),
                shiny::tags$td(det$estado[i])
              )
            })

            tabla_carreras <- shiny::tags$table(
              class = "table table-condensed table-striped",
              shiny::tags$thead(
                shiny::tags$tr(
                  shiny::tags$th("Carrera"),
                  shiny::tags$th("Tipo pensum"),
                  shiny::tags$th("Inscrito"),
                  shiny::tags$th("Cierre"),
                  shiny::tags$th("Graduación"),
                  shiny::tags$th("Estado")
                )
              ),
              shiny::tags$tbody(filas)
            )
          }
        }
      }

      shiny::tagList(
        shiny::h5("Información Académica:"),
        shiny::tags$table(
          class = "table table-condensed",
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("ID:")), shiny::tags$td(estudiante_id)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Carreras registradas:")), shiny::tags$td(carreras_txt)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Fecha de inscripción (global):")), shiny::tags$td(fecha_inscripcion_txt)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Fecha de cierre (global):")), shiny::tags$td(fecha_cierre_txt)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Fecha de graduación (global):")), shiny::tags$td(fecha_graduacion_txt)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Estado académico (global):")), shiny::tags$td(estado_academico_txt)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Cursos tomados (únicos):")), shiny::tags$td(cursos_tomados)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Cursos aprobados (nota ≥ 61):")), shiny::tags$td(cursos_aprobados)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Tasa de aprobación:")), shiny::tags$td(ifelse(is.na(tasa_aprobacion), "N/A", paste0(tasa_aprobacion, "%")))),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Promedio (solo aprobados):")), shiny::tags$td(ifelse(is.na(promedio_aprobados), "N/A", promedio_aprobados))),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Años activo:")), shiny::tags$td(años_range)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Modalidades:")), shiny::tags$td(modalidades)),
          shiny::tags$tr(shiny::tags$td(shiny::tags$b("Períodos:")), shiny::tags$td(periodos))
        ),
        if (!is.null(tabla_carreras)) shiny::tagList(
          shiny::hr(),
          shiny::h5("Detalle por carrera (datos complementarios):"),
          tabla_carreras
        )
      )
    })
    
    # Tabla de historial (optimizada)
    output$tabla_historial <- DT::renderDT({
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        return(DT::datatable(data.frame(Mensaje = "No hay datos disponibles"),
                             options = list(dom = 't')))
      }
      
      # Preparar datos de manera eficiente
      if (is.data.table(datos)) {
        if ("nombrecurso" %in% names(datos)) {
          datos_tabla <- datos[order(-ano, curso),
                               .(Año = ano,
                                 Período = periodo,
                                 Curso = curso,
                                 `Nombre del curso` = nombrecurso,
                                 Nota = nota,
                                 Modalidad = modalidad)]
        } else {
          datos_tabla <- datos[order(-ano, curso),
                               .(Año = ano,
                                 Período = periodo,
                                 Curso = curso,
                                 Nota = nota,
                                 Modalidad = modalidad)]
        }
      } else {
        if ("nombrecurso" %in% names(datos)) {
          datos_tabla <- datos %>%
            dplyr::arrange(dplyr::desc(ano), curso) %>%
            dplyr::select(ano, periodo, curso, nombrecurso, nota, modalidad) %>%
            dplyr::rename(Año = ano, Período = periodo, Curso = curso,
                          `Nombre del curso` = nombrecurso, Nota = nota, Modalidad = modalidad)
        } else {
          datos_tabla <- datos %>%
            dplyr::arrange(dplyr::desc(ano), curso) %>%
            dplyr::select(ano, periodo, curso, nota, modalidad) %>%
            dplyr::rename(Año = ano, Período = periodo, Curso = curso,
                          Nota = nota, Modalidad = modalidad)
        }
      }

      DT::datatable(
        datos_tabla,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json'
          ),
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        filter = 'top',
        class = 'display compact'
      )
    })
    
    # Descargar reporte
    output$descargar_reporte <- shiny::downloadHandler(
      filename = function() {
        paste0("reporte_estudiante_", Sys.Date(), ".csv")
      },
      content = function(file) {
        datos <- datos_estudiante()
        if (nrow(datos) == 0) {
          write.csv(data.frame(Mensaje = "No hay datos disponibles"), file)
        } else {
          if (is.data.table(datos)) {
            datos_export <- datos[order(-ano, curso), 
                                  .(Año = ano, 
                                    Período = periodo, 
                                    Curso = curso, 
                                    Nota = nota, 
                                    Modalidad = modalidad)]
          } else {
            datos_export <- datos %>%
              dplyr::arrange(dplyr::desc(ano), curso) %>%
              dplyr::select(ano, periodo, curso, nota, modalidad) %>%
              dplyr::rename(Año = ano, Período = periodo, Curso = curso, 
                            Nota = nota, Modalidad = modalidad)
          }
          readr::write_csv(datos_export, file)
        }
      }
    )
    
    # Comparar con promedio
    shiny::observeEvent(input$comparar_estudiante, {
      datos <- datos_estudiante()
      if (nrow(datos) == 0) {
        notify_user("No hay datos del estudiante para comparar", 
                    tipo = "warning", duration = 3)
        return()
      }
      
      shiny::showModal(shiny::modalDialog(
        title = "Comparativa con Promedio General",
        shiny::tags$p("Esta funcionalidad compara el rendimiento del estudiante con el promedio institucional."),
        shiny::tags$p("En desarrollo..."),
        easyClose = TRUE,
        footer = shiny::modalButton("Cerrar")
      ))
    })
  })
}

# ------------------------------------------------------------------------------
# INTERFAZ PRINCIPAL OPTIMIZADA
# ------------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  skin = "blue",
  
  # Encabezado
  header = shinydashboard::dashboardHeader(
    title = shiny::tags$div(
      style = "display: flex; align-items: center;",
      shiny::tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Logo_USAC.svg/1200px-Logo_USAC.svg.png",
                      height = "35px", style = "margin-right: 15px;"),
      shiny::tags$div(
        shiny::tags$h4("Tablero FIUSAC V7.0 - Análisis Académico", 
                       style = "margin: 0; font-weight: bold; color: #0056a6;"),
        shiny::tags$p("Análisis Individual, Institucional y Series de Tiempo", 
                      style = "margin: 0; font-size: 12px; color: #666;")
      )
    ),
    titleWidth = 500
  ),
  
  # Barra lateral optimizada
  sidebar = shinydashboard::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      id = "menu_principal",
      shinydashboard::menuItem("🏠 Inicio", tabName = "inicio", icon = shiny::icon("home")),
      shinydashboard::menuItem("👤 Análisis Individual", tabName = "individual", 
                               icon = shiny::icon("user-graduate")),
      shinydashboard::menuItem("🏫 Análisis Institucional", tabName = "institucional", 
                               icon = shiny::icon("university")),
      shinydashboard::menuItem("📈 Series de Tiempo", tabName = "series", 
                               icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("📋 Datos", tabName = "datos", icon = shiny::icon("database")),
      shinydashboard::menuItem("📤 Exportar", tabName = "exportar", icon = shiny::icon("download"))
    ),
    
    shiny::hr(),
    
    # Panel de información del sistema
    shiny::div(
      style = "padding: 15px; background-color: #f8f9fa;",
      shiny::h5("ℹ️ Información del Sistema"),
      shiny::verbatimTextOutput("info_sistema"),
      shiny::hr(),
      shiny::actionButton("optimizar_memoria", "Optimizar Memoria", 
                          icon = shiny::icon("memory"), 
                          class = "btn-sm btn-block",
                          style = "margin-bottom: 10px;")
    )
  ),
  
  # Cuerpo principal
  body = shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste0(
        "/* Estilos optimizados */\n",
        "body { \n",
        "  font-family: 'Segoe UI', 'Helvetica Neue', sans-serif; \n",
        "  font-size: 14px;\n",
        "}\n",
        ".content-wrapper, .right-side { \n",
        "  background-color: #f5f7fa; \n",
        "}\n",
        "\n",
        "/* Cards optimizadas */\n",
        ".small-box { \n",
        "  border-radius: 6px; \n",
        "  margin-bottom: 10px;\n",
        "}\n",
        ".box { \n",
        "  border-radius: 6px; \n",
        "  box-shadow: 0 2px 4px rgba(0,0,0,0.05);\n",
        "  margin-bottom: 15px;\n",
        "}\n",
        "\n",
        "/* Tablas más ligeras */\n",
        ".dataTables_wrapper { \n",
        "  font-size: 12px; \n",
        "}\n",
        "table.dataTable {\n",
        "  width: 100% !important;\n",
        "}\n",
        "\n",
        "/* Mejoras para responsive */\n",
        "@media (max-width: 768px) {\n",
        "  .box { margin-bottom: 10px; }\n",
        "  .tab-content > .tab-pane { padding: 10px; }\n",
        "  .col-md-6, .col-md-4, .col-md-3, .col-md-8, .col-md-12 {\n",
        "    padding-left: 5px;\n",
        "    padding-right: 5px;\n",
        "  }\n",
        "}\n",
        "\n",
        "/* Botones mejorados */\n",
        ".btn-block {\n",
        "  width: 100%;\n",
        "  margin-bottom: 5px;\n",
        "}\n",
        "\n",
        "/* Spinners personalizados */\n",
        ".shiny-spinner-output-container {\n",
        "  text-align: center;\n",
        "  padding: 20px;\n",
        "}\n"
      )))
    ),
    
    shinydashboard::tabItems(
      # Pestaña Inicio
      shinydashboard::tabItem(
        tabName = "inicio",
        shiny::h2("Bienvenido al Tablero FIUSAC V7.0"),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            shiny::HTML('
            <div style="padding: 20px; text-align: center;">
              <h3 style="color: #0056a6;">Sistema de Análisis Académico Optimizado</h3>
              <p class="lead">Análisis integral del rendimiento académico 2001-2024</p>
              
              <div class="row" style="margin-top: 30px;">
                <div class="col-md-4">
                  <div class="panel panel-default">
                    <div class="panel-body" style="padding: 20px;">
                      <h4><i class="fa fa-user-graduate" style="color: #3498db;"></i> Análisis Individual</h4>
                      <p>Perfil detallado de cada estudiante, historial académico completo y proyecciones.</p>
                    </div>
                  </div>
                </div>
                <div class="col-md-4">
                  <div class="panel panel-default">
                    <div class="panel-body" style="padding: 20px;">
                      <h4><i class="fa fa-university" style="color: #2ecc71;"></i> Análisis Institucional</h4>
                      <p>Métricas globales, comparativas entre cursos y modalidades, análisis de rendimiento.</p>
                    </div>
                  </div>
                </div>
                <div class="col-md-4">
                  <div class="panel panel-default">
                    <div class="panel-body" style="padding: 20px;">
                      <h4><i class="fa fa-chart-line" style="color: #e74c3c;"></i> Series de Tiempo</h4>
                      <p>Evolución histórica, tendencias, cohortes y análisis de estacionalidad.</p>
                    </div>
                  </div>
                </div>
              </div>
              
              <hr style="margin: 40px 0;">
              
              <div class="alert alert-info" style="text-align: left;">
                <h4><i class="fa fa-bolt"></i> Optimizaciones Implementadas:</h4>
                <ul>
                  <li><strong>Carga diferida:</strong> Los datos se cargan solo cuando son necesarios</li>
                  <li><strong>Memoización:</strong> Cálculos repetitivos se almacenan en caché</li>
                  <li><strong>Limpieza automática:</strong> Objetos temporales se eliminan automáticamente</li>
                  <li><strong>Filtros inteligentes:</strong> Solo aplican a secciones activas</li>
                  <li><strong>Data.table:</strong> Procesamiento ultra rápido de datos</li>
                </ul>
              </div>
            </div>
            ')
          )
        )
      ),
      
      # Pestaña Análisis Individual
      shinydashboard::tabItem(
        tabName = "individual",
        shiny::fluidRow(
          shiny::column(12, mod_filtros_ui("filtros_individual"))
        ),
        shiny::fluidRow(
          shiny::column(12, mod_individual_ui("individual"))
        )
      ),
      
      # Pestaña Análisis Institucional
      # Pestaña Análisis Institucional
shinydashboard::tabItem(
  tabName = "institucional",
  shiny::h2("🏫 Análisis Institucional - Visión Global"),

  # ---------------------------
  # Filtros Institucionales
  # ---------------------------
  shinydashboard::box(
    title = "🎛️ Filtros Institucionales (se recalcula al presionar 'Aplicar filtros')",
    status = "primary",
    solidHeader = TRUE,
    width = 12,

    shiny::fluidRow(
      shiny::column(
        3,
        shiny::sliderInput(
          "inst_rango_anos",
          "Rango de años",
          min = 2001, max = 2024, value = c(2001, 2024), step = 1, sep = ""
        )
      ),
      shiny::column(
        3,
        shiny::selectizeInput(
          "inst_cohortes",
          "Cohortes (año de ingreso)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todas')
        )
      ),
      shiny::column(
        3,
        shiny::selectizeInput(
          "inst_modalidades",
          "Modalidades",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todas')
        )
      ),
      shiny::column(
        3,
        shiny::selectizeInput(
          "inst_periodos",
          "Tipo de período/ciclo",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todos')
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectizeInput(
          "inst_cursos",
          "Cursos",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todos', maxItems = 200)
        )
      ),
      shiny::column(
        3,
        shiny::selectizeInput(
          "inst_tipo_pensum",
          "Tipo de pensum",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todos')
        )
      ),
      shiny::column(
        3,
        shiny::selectizeInput(
          "inst_carreras",
          "Carrera",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todas')
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectizeInput(
          "inst_departamentos",
          "Departamento",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todos')
        )
      ),
      shiny::column(
        4,
        shiny::selectizeInput(
          "inst_municipios",
          "Municipio",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Todos')
        )
      ),
      shiny::column(
        2,
        shiny::checkboxInput("inst_solo_aprobados", "Solo aprobados (≥ 61)", value = FALSE)
      ),
      shiny::column(
        2,
        shiny::actionButton(
          "inst_aplicar_filtros",
          "Aplicar filtros",
          icon = shiny::icon("filter"),
          class = "btn-primary"
        )
      )
    ),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(
        3,
        shiny::numericInput(
          "param_abandono_anos",
          "Parámetro: años para abandono",
          value = 2, min = 1, max = 10, step = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          "param_tiempo_estandar_anos",
          "Parámetro: tiempo estándar (años)",
          value = 6, min = 1, max = 15, step = 1
        )
      ),
      shiny::column(
        6,
        shiny::uiOutput("inst_resumen_filtros"),
        shiny::uiOutput("inst_diag_filtros")
      )
    )
  ),

  # ---------------------------
  # KPIs Institucionales
  # ---------------------------
  shiny::fluidRow(
    shinydashboard::valueBoxOutput("kpi_inst_estudiantes", width = 2),
    shinydashboard::valueBoxOutput("kpi_inst_cursos", width = 2),
    shinydashboard::valueBoxOutput("kpi_inst_aprobacion", width = 2),
    shinydashboard::valueBoxOutput("kpi_inst_reprobacion", width = 2),
    shinydashboard::valueBoxOutput("kpi_inst_graduados", width = 2),
    shinydashboard::valueBoxOutput("kpi_inst_abandono", width = 2)
  ),

  # ---------------------------
  # Visualizaciones
  # ---------------------------
  shinydashboard::tabBox(
    title = "📊 Visualizaciones Institucionales",
    width = 12,

    shiny::tabPanel(
      "Aprobación/Reprobación",
      plotly::plotlyOutput("inst_g_aprob_reprob_anual") %>% shinycssloaders::withSpinner(color = "#3498db"),
      shiny::hr(),
      plotly::plotlyOutput("inst_g_aprob_reprob_ciclo") %>% shinycssloaders::withSpinner(color = "#3498db")
    ),

    shiny::tabPanel(
      "Deserción (t+1 por ciclo)",
      plotly::plotlyOutput("inst_g_desercion_ciclo") %>% shinycssloaders::withSpinner(color = "#3498db"),
      shiny::hr(),
      DT::DTOutput("inst_tabla_desercion") %>% shinycssloaders::withSpinner(color = "#3498db")
    ),

    shiny::tabPanel(
      "Heatmap aprobación",
      plotly::plotlyOutput("inst_heatmap_aprob") %>% shinycssloaders::withSpinner(color = "#3498db")
    ),

    shiny::tabPanel(
      "Cohortes y eficiencia terminal",
      plotly::plotlyOutput("inst_g_cohortes") %>% shinycssloaders::withSpinner(color = "#3498db"),
      shiny::hr(),
      DT::DTOutput("inst_tabla_cohortes") %>% shinycssloaders::withSpinner(color = "#3498db")
    ),

    shiny::tabPanel(
      "Detalle",
      DT::DTOutput("inst_tabla_detalle") %>% shinycssloaders::withSpinner(color = "#3498db")
    )
  )
),

      # Pestaña Datos
      shinydashboard::tabItem(
        tabName = "datos",
        shiny::h2("📋 Datos Completos"),
        shiny::fluidRow(
          shiny::column(12, mod_filtros_ui("filtros_datos"))
        ),
        shiny::fluidRow(
          shiny::column(12,
                        DT::DTOutput("tabla_datos") %>% 
                          shinycssloaders::withSpinner(color = "#3498db")
          )
        )
      ),
      
      # Pestaña Exportar
      shinydashboard::tabItem(
        tabName = "exportar",
        shiny::h2("📤 Exportación de Datos"),
        shiny::fluidRow(
          shinydashboard::box(
            title = "📊 Exportar Datos Completos",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            shiny::h4("Dataset completo"),
            shiny::p("Incluye todos los registros históricos"),
            shiny::br(),
            shiny::downloadButton("exportar_csv", "CSV", 
                                  class = "btn-primary btn-block",
                                  style = "margin-bottom: 10px;"),
            shiny::downloadButton("exportar_excel", "Excel", 
                                  class = "btn-success btn-block")
          ),
          shinydashboard::box(
            title = "📈 Exportar Datos Filtrados",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            shiny::h4("Datos con filtros aplicados"),
            shiny::p("Incluye solo los datos visibles actualmente"),
            shiny::br(),
            shiny::downloadButton("exportar_filtrados", "CSV Filtrado", 
                                  class = "btn-info btn-block",
                                  style = "margin-bottom: 10px;"),
            shiny::downloadButton("exportar_reporte", "Reporte Completo", 
                                  class = "btn-warning btn-block")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVIDOR OPTIMIZADO
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Variables globales reactivas
  datos_crudos <- shiny::reactiveVal(NULL)
  datos_activos <- shiny::reactiveVal(NULL)
  datos_complementarios_rv <- shiny::reactiveVal(NULL)

  
  # Inicialización optimizada
  shiny::observe({
    tryCatch({
      log_message("info", "Iniciando carga de datos...")
      
      # Reusar dataset global si ya fue cargado (evita recarga por sesión)
      # + Invalida caché si el archivo cambió en disco.
      archivo_historico <- resolver_ruta_datos()
      mtime_actual <- tryCatch(file.info(archivo_historico)$mtime, error = function(e) NA)
      
      if (exists("datos", envir = .datos_cache_env, inherits = FALSE) &&
          exists("datos_mtime", envir = .datos_cache_env, inherits = FALSE) &&
          isTRUE(identical(get("datos_mtime", envir = .datos_cache_env, inherits = FALSE), mtime_actual))) {
        datos <- get("datos", envir = .datos_cache_env, inherits = FALSE)
      } else {
        datos <- cargar_datos_memoised()
        assign("datos", datos, envir = .datos_cache_env)
        assign("datos_mtime", mtime_actual, envir = .datos_cache_env)
      }
      
      datos_crudos(datos)
      datos_activos(datos)  # Inicialmente usar todos los datos

      # Cargar datos complementarios con caché por fecha de modificación
      archivo_comp <- resolver_ruta_complementarios()
      mtime_comp <- if (!is.null(archivo_comp)) tryCatch(file.info(archivo_comp)$mtime, error = function(e) NA) else NA

      if (!is.null(archivo_comp) &&
          exists("comp", envir = .datos_cache_env, inherits = FALSE) &&
          exists("comp_mtime", envir = .datos_cache_env, inherits = FALSE) &&
          isTRUE(identical(get("comp_mtime", envir = .datos_cache_env, inherits = FALSE), mtime_comp))) {
        comp <- get("comp", envir = .datos_cache_env, inherits = FALSE)
      } else if (!is.null(archivo_comp)) {
        comp <- cargar_complementarios_optimizado()
        assign("comp", comp, envir = .datos_cache_env)
        assign("comp_mtime", mtime_comp, envir = .datos_cache_env)
      } else {
        comp <- NULL
      }

      datos_complementarios_rv(comp)

      
      notify_user("Datos cargados correctamente", tipo = "message", duracion = 5)
      
    }, error = function(e) {
      notify_user(paste("Error al cargar datos:", e$message), tipo = "error", duracion = 10)
    })
  })
  
  # Información del sistema
  output$info_sistema <- shiny::renderText({
    datos <- datos_crudos()
    
    if (is.null(datos)) {
      return("Cargando datos...")
    }
    
    tryCatch({
      n_reg <- nrow(datos)
      n_est <- if ("identificador" %in% names(datos)) length(unique(datos$identificador)) else NA_integer_
      n_cur <- if ("curso" %in% names(datos)) length(unique(datos$curso)) else NA_integer_
      
      anos_txt <- if ("ano" %in% names(datos)) {
        a <- suppressWarnings(as.integer(datos$ano))
        a <- a[!is.na(a)]
        if (length(a) > 0) paste(min(a), "-", max(a)) else "N/D"
      } else {
        "N/D"
      }
      
      paste0(
        "Memoria usada: ", format(utils::object.size(datos), units = "MB"),
        "\nRegistros: ", format(n_reg, big.mark = ","),
        "\nEstudiantes: ", ifelse(is.na(n_est), "N/D", format(n_est, big.mark = ",")),
        "\nCursos: ", ifelse(is.na(n_cur), "N/D", format(n_cur, big.mark = ",")),
        "\nAños: ", anos_txt,
        "\nÚltima actualización: ", format(Sys.time(), "%H:%M:%S")
      )
    }, error = function(e) {
      paste("Información del sistema no disponible:", e$message)
    })
  })
  
  # Optimizar memoria
  shiny::observeEvent(input$optimizar_memoria, {
    # Forzar garbage collection
    gc(verbose = FALSE, full = TRUE)
    
    notify_user("Memoria optimizada", tipo = "default", duracion = 3)
  })
  
  # Filtros globales
  filtros_individual <- mod_filtros_server("filtros_individual", datos_crudos)
  filtros_datos <- mod_filtros_server("filtros_datos", datos_crudos)
  
  # Seleccionar filtros según pestaña activa
  filtros <- shiny::reactive({
    if (is.null(input$menu_principal)) {
      return(filtros_individual())
    }
    if (identical(input$menu_principal, "datos")) {
      return(filtros_datos())
    }
    return(filtros_individual())
  })
  
  # Observar cambios en los filtros y actualizar datos activos
  shiny::observe({
    shiny::req(datos_crudos(), filtros())
    
    tryCatch({
      datos <- data.table::as.data.table(datos_crudos())
      filtros_aplicar <- filtros()
      
      # Si no hay filtros aplicados, usar todos los datos
      if ((is.null(filtros_aplicar$rango_anos) || length(filtros_aplicar$rango_anos) == 0) &&
          (is.null(filtros_aplicar$modalidad) || length(filtros_aplicar$modalidad) == 0) &&
          (is.null(filtros_aplicar$periodo) || length(filtros_aplicar$periodo) == 0) &&
          (is.null(filtros_aplicar$curso) || length(filtros_aplicar$curso) == 0) &&
          (is.null(filtros_aplicar$estudiante) || filtros_aplicar$estudiante == "")) {
        datos_activos(datos)
        return()
      }
      
      # Aplicar filtros de manera eficiente con data.table (sin duplicar todo el dataset)
      idx <- rep(TRUE, nrow(datos))

      if (isTRUE(identical(filtros_aplicar$seccion, "individual"))) {

        # En análisis individual, mostrar HISTORIAL COMPLETO del estudiante:
        # se filtra únicamente por estudiante y se ignoran filtros de año/modalidad/período/curso
        estudiante_id <- filtros_aplicar$estudiante
        id_manual <- filtros_aplicar$id_manual

        if (!is.null(estudiante_id) && estudiante_id != "" && "identificador" %in% names(datos)) {
          idx <- idx & (datos$identificador == estudiante_id)
        } else if (!is.null(id_manual) && id_manual != "" && "identificador" %in% names(datos)) {
          idx <- idx & (datos$identificador == id_manual)
        }

      } else {

        # Filtro por años
        if (!is.null(filtros_aplicar$rango_anos) && length(filtros_aplicar$rango_anos) > 0) {
          anos_sel <- suppressWarnings(as.integer(filtros_aplicar$rango_anos))
          anos_sel <- anos_sel[!is.na(anos_sel)]
          if (length(anos_sel) > 0 && "ano" %in% names(datos)) {
            idx <- idx & (datos$ano %in% anos_sel)
          }
        }

        # Filtro por modalidad
        if (!is.null(filtros_aplicar$modalidad) && length(filtros_aplicar$modalidad) > 0) {
          if ("modalidad" %in% names(datos)) {
            idx <- idx & (datos$modalidad %in% filtros_aplicar$modalidad)
          }
        }

        # Filtro por periodo
        if (!is.null(filtros_aplicar$periodo) && length(filtros_aplicar$periodo) > 0) {
          if ("periodo" %in% names(datos)) {
            idx <- idx & (datos$periodo %in% filtros_aplicar$periodo)
          }
        }

        # Filtro por curso
        if (!is.null(filtros_aplicar$curso) && length(filtros_aplicar$curso) > 0) {
          if ("curso" %in% names(datos)) {
            idx <- idx & (datos$curso %in% filtros_aplicar$curso)
          }
        }
      }

      # Aplicar filtro final
      if (any(idx)) {
        datos_activos(datos[idx])
      } else {
        datos_activos(datos[0])
      }
    }, error = function(e) {
      notify_user(paste0('Error al aplicar filtros: ', e$message), tipo = 'error', duracion = 8)
      # Fallback seguro: mantener datos sin filtrar
      datos_activos(datos_crudos())
    })
  })
  
  # Módulos de análisis
  mod_individual_server("individual", shiny::reactive({ datos_activos() }), datos_complementarios = shiny::reactive({ datos_complementarios_rv() }), filtros_modulo_id = "filtros_individual")
  

  # Tabla de datos completa (optimizada)
  output$tabla_datos <- DT::renderDT({
    shiny::req(datos_activos())
    
    tryCatch({
      datos <- datos_activos()
      
      # Limitar la visualización a 1000 registros para mejor rendimiento
      if (nrow(datos) > 1000) {
        datos <- datos[1:1000, ]
      }
      
      DT::datatable(
        datos,
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json'
          ),
          scrollX = TRUE,
          scrollY = "400px",
          deferRender = TRUE,
          scroller = TRUE
        ),
        extensions = c('Buttons', 'Scroller'),
        rownames = FALSE,
        filter = 'top',
        class = 'display compact stripe'
      )
    }, error = function(e) {
      notify_user(paste0('Error al generar tabla de datos: ', e$message), tipo = 'error', duracion = 8)
      DT::datatable(data.frame(Mensaje = 'No se pudo cargar la tabla.'), options = list(dom = 't'))
    })
  })
  
  # Funciones de exportación optimizadas
  output$exportar_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("fiusac_datos_completos_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(datos_crudos())
      data.table::fwrite(datos_crudos(), file)
    }
  )
  
  output$exportar_excel <- shiny::downloadHandler(
    filename = function() {
      paste0("fiusac_datos_completos_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      shiny::req(datos_crudos())
      writexl::write_xlsx(datos_crudos(), file)
    }
  )
  
  output$exportar_filtrados <- shiny::downloadHandler(
    filename = function() {
      paste0("fiusac_datos_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(datos_activos())
      data.table::fwrite(datos_activos(), file)
    }
  )
  
  output$exportar_reporte <- shiny::downloadHandler(
    filename = function() {
      paste0("reporte_fiusac_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(datos_activos())
      
      # Crear reporte resumido eficientemente
      if (is.data.table(datos_activos())) {
        reporte <- datos_activos()[
          !is.na(nota),
          .(Estudiantes = uniqueN(identificador),
            Cursos = uniqueN(curso),
            Promedio = round(mean(nota, na.rm = TRUE), 2),
            Tasa_Aprobacion = paste0(round(mean(nota >= 61, na.rm = TRUE) * 100, 1), "%"),
            Registros = .N)
        ]
      } else {
        reporte <- datos_activos() %>%
          dplyr::filter(!is.na(nota)) %>%
          dplyr::summarise(
            Estudiantes = dplyr::n_distinct(identificador),
            Cursos = dplyr::n_distinct(curso),
            Promedio = round(mean(nota, na.rm = TRUE), 2),
            Tasa_Aprobacion = paste0(round(mean(nota >= 61, na.rm = TRUE) * 100, 1), "%"),
            Registros = dplyr::n()
          )
      }
      
      readr::write_csv(reporte, file)
    }
  )
  
  # Limpieza al cerrar sesión
  session$onSessionEnded(function() {
    log_message("info", "Sesión finalizada - Limpiando recursos")
    # Ejecutar garbage collection
    try(gc(verbose = FALSE), silent = TRUE)
  })
}

# ------------------------------------------------------------------------------
# VERIFICACIÓN Y EJECUCIÓN
# ------------------------------------------------------------------------------

# Función para verificar requisitos antes de ejecutar
verificar_requisitos <- function() {
  log_message("titulo", "VERIFICACIÓN DE REQUISITOS - TABLERO FIUSAC V7.0")
  
  # 1. Verificar si la carpeta datos existe
  if (!dir.exists("datos")) {
    dir.create("datos", recursive = TRUE)
    log_message("info", "Creada carpeta 'datos/'")
  }
  
  # 2. Verificar archivo de datos
  archivo_datos <- resolver_ruta_datos()
  if (!file.exists(archivo_datos)) {
    log_message("error", paste("Archivo de datos no encontrado:", archivo_datos))
    log_message("info", "Por favor, coloque 'datos_2001_2024.csv' en '01_data/' (recomendado) o en '03_shiny_app/datos/' (compatibilidad)")
    return(FALSE)
  }
  
  # 3. Verificar paquetes necesarios
  paquetes_necesarios <- c("shiny", "shinydashboard", "shinycssloaders", "shinyjs", "dplyr", "tidyr", "data.table", "readr", "janitor", "plotly", "ggplot2", "DT", "lubridate", "stringr", "scales", "purrr", "memoise", "digest", "writexl")
  paquetes_faltantes <- paquetes_necesarios[!sapply(paquetes_necesarios, requireNamespace, quietly = TRUE)]
  
  if (length(paquetes_faltantes) > 0) {
    log_message("warning", paste("Paquetes faltantes:", paste(paquetes_faltantes, collapse = ", ")))
    log_message("info", paste("Ejecute: install.packages(c('", paste(paquetes_faltantes, collapse = "', '"), "'))", sep = ""))
    return(FALSE)
  }
  
  log_message("info", "✅ Todos los requisitos verificados correctamente")
  return(TRUE)
}

# Ejecutar verificación y aplicación
# Ejecutar verificación y aplicación
if (interactive()) {
  if (verificar_requisitos()) {
    log_message("titulo", "INICIANDO APLICACIÓN SHINY")
    log_message("info", "Presione Ctrl+C para detener la aplicación")
    
    shiny::shinyApp(ui = ui, server = server)
  }
} else {
  shiny::shinyApp(ui = ui, server = server)
}

