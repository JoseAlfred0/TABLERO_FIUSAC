# R/modules/filtros/server.R
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