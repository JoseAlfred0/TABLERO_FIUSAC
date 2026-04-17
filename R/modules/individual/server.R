# R/modules/individual/server.R
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