# Módulo: Series de Tiempo
mod_series_server <- function(input, output, session, inst_dt_base, inst_estado_estudiante) {
  ser_diag_rv <- shiny::reactiveVal(NULL)
  
  ser_dt_base <- shiny::reactive({
    inst_dt_base()
  })
  
  # Inicializar controles con valores disponibles
  shiny::observeEvent(ser_dt_base(), {
    dt <- ser_dt_base()
    if (is.null(dt) || nrow(dt) == 0) return()
    if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
    
    anos_min <- suppressWarnings(min(dt$ano, na.rm = TRUE))
    anos_max <- suppressWarnings(max(dt$ano, na.rm = TRUE))
    if (is.finite(anos_min) && is.finite(anos_max)) {
      shiny::updateSliderInput(
        session,
        "ser_rango_anos",
        min = anos_min,
        max = anos_max,
        value = c(anos_min, anos_max)
      )
    }
    
    mods <- sort(unique(na.omit(dt$modalidad)))
    per <- sort(unique(na.omit(dt$periodo_tipo)))
    cursos <- sort(unique(na.omit(dt$curso_label)))
    
    shiny::updateSelectizeInput(session, "ser_modalidades", choices = mods, selected = NULL, server = TRUE)
    shiny::updateSelectizeInput(session, "ser_periodos", choices = per, selected = NULL, server = TRUE)
    shiny::updateSelectizeInput(session, "ser_cursos", choices = cursos, selected = NULL, server = TRUE)
    
    if ("tipo_pensum" %in% names(dt)) {
      shiny::updateSelectizeInput(
        session,
        "ser_tipo_pensum",
        choices = sort(unique(na.omit(dt$tipo_pensum))),
        selected = NULL,
        server = TRUE
      )
    }
    if ("carrera" %in% names(dt)) {
      shiny::updateSelectizeInput(
        session,
        "ser_carreras",
        choices = sort(unique(na.omit(dt$carrera))),
        selected = NULL,
        server = TRUE
      )
    }
    if ("departamento" %in% names(dt)) {
      shiny::updateSelectizeInput(
        session,
        "ser_departamentos",
        choices = sort(unique(na.omit(dt$departamento))),
        selected = NULL,
        server = TRUE
      )
    }
    if ("municipio" %in% names(dt)) {
      shiny::updateSelectizeInput(
        session,
        "ser_municipios",
        choices = sort(unique(na.omit(dt$municipio))),
        selected = NULL,
        server = TRUE
      )
    }
    
    # Cohortes: derivadas del primer año con cursos en el historial
    if (!("cohorte" %in% names(dt))) {
      coh_map <- dt[!is.na(identificador) & !is.na(ano),
                    .(cohorte = min(ano, na.rm = TRUE)),
                    by = identificador
      ]
      dt <- merge(dt, coh_map, by = "identificador", all.x = TRUE)
    }
    coh_choices <- sort(unique(na.omit(dt$cohorte)))
    shiny::updateSelectizeInput(session, "ser_cohortes", choices = coh_choices, selected = NULL, server = TRUE)
  }, ignoreInit = FALSE)
  
  # Municipios dependientes del/los departamento(s) seleccionado(s)
  shiny::observeEvent(input$ser_departamentos, {
    dt <- ser_dt_base()
    if (is.null(dt) || nrow(dt) == 0) return()
    if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
    
    deps_sel <- input$ser_departamentos
    if (is.null(deps_sel) || length(deps_sel) == 0) {
      mun_choices <- sort(unique(na.omit(dt$municipio)))
    } else {
      mun_choices <- sort(unique(na.omit(dt[departamento %in% deps_sel, municipio])))
    }
    shiny::updateSelectizeInput(session, "ser_municipios", choices = mun_choices, selected = NULL, server = TRUE)
  }, ignoreInit = TRUE)
  
  ser_filtros_aplicados <- shiny::eventReactive(input$ser_aplicar_filtros, {
    list(
      rango_anos = input$ser_rango_anos,
      cohortes = input$ser_cohortes,
      modalidades = input$ser_modalidades,
      periodos = input$ser_periodos,
      cursos = input$ser_cursos,
      tipo_pensum = input$ser_tipo_pensum,
      carreras = input$ser_carreras,
      departamentos = input$ser_departamentos,
      municipios = input$ser_municipios,
      promedio_solo_aprobados = isTRUE(input$ser_promedio_solo_aprobados),
      suavizado = isTRUE(input$ser_suavizado),
      granularidad = input$ser_granularidad,
      indicador = input$ser_indicador
    )
  })
  
  ser_datos_filtrados <- shiny::eventReactive(input$ser_aplicar_filtros, {
    dt <- ser_dt_base()
    if (is.null(dt) || nrow(dt) == 0) return(NULL)
    if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
    # Asegurar tipos y cohorte (Series de Tiempo): evita errores y filtros vacíos
    if ("ano" %in% names(dt)) dt[, ano := suppressWarnings(as.integer(ano))]
    if ("nota" %in% names(dt)) dt[, nota := suppressWarnings(as.numeric(nota))]
    if ("identificador" %in% names(dt)) dt[, identificador := as.character(identificador)]
    if ("cohorte" %in% names(dt)) dt[, cohorte := suppressWarnings(as.integer(cohorte))]
    if (!("cohorte" %in% names(dt)) && all(c("identificador", "ano") %in% names(dt))) {
      dt[, cohorte := {
        v <- suppressWarnings(min(ano, na.rm = TRUE))
        if (is.infinite(v)) NA_integer_ else as.integer(v)
      }, by = identificador]
    }
    
    diag <- list(base_rows = nrow(dt))
    
    # Cohorte (año de ingreso) desde el primer año con cursos
    if (!("cohorte" %in% names(dt))) {
      coh_map <- dt[!is.na(identificador) & !is.na(ano),
                    .(cohorte = min(ano, na.rm = TRUE)),
                    by = identificador
      ]
      dt <- merge(dt, coh_map, by = "identificador", all.x = TRUE)
    }
    
    # Rango de años
    r <- input$ser_rango_anos
    if (!is.null(r) && length(r) == 2) {
      dt <- dt[ano >= r[1] & ano <= r[2]]
      diag$rango_anos <- paste0(r[1], "-", r[2])
    }
    
    if (!is.null(input$ser_cohortes) && length(input$ser_cohortes) > 0) {
      if (!is.null(input$ser_cohortes) && length(input$ser_cohortes) > 0 && ! ("Todas" %in% input$ser_cohortes)) {
        dt <- dt[cohorte %in% suppressWarnings(as.integer(input$ser_cohortes))]
      }
    }
    if (!is.null(input$ser_modalidades) && length(input$ser_modalidades) > 0) {
      if (!is.null(input$ser_modalidades) && length(input$ser_modalidades) > 0 && ! ("Todas" %in% input$ser_modalidades)) {
        dt <- dt[modalidad %in% input$ser_modalidades]
      }
    }
    if (!is.null(input$ser_periodos) && length(input$ser_periodos) > 0 && ! ("Todos" %in% input$ser_periodos)) {
      dt <- dt[periodo_tipo %in% input$ser_periodos]
    }
    if (!is.null(input$ser_cursos) && length(input$ser_cursos) > 0) {
      dt <- dt[curso_label %in% input$ser_cursos]
    }
    if (!is.null(input$ser_tipo_pensum) && length(input$ser_tipo_pensum) > 0 && "tipo_pensum" %in% names(dt)) {
      dt <- dt[tipo_pensum %in% input$ser_tipo_pensum]
    }
    if (!is.null(input$ser_carreras) && length(input$ser_carreras) > 0 && "carrera" %in% names(dt)) {
      dt <- dt[carrera %in% input$ser_carreras]
    }
    if (!is.null(input$ser_departamentos) && length(input$ser_departamentos) > 0 && "departamento" %in% names(dt)) {
      dt <- dt[departamento %in% input$ser_departamentos]
    }
    if (!is.null(input$ser_municipios) && length(input$ser_municipios) > 0 && "municipio" %in% names(dt)) {
      dt <- dt[municipio %in% input$ser_municipios]
    }
    
    diag$filtrado_rows <- nrow(dt)
    diag$estudiantes <- if (nrow(dt) > 0) data.table::uniqueN(dt$identificador) else 0L
    diag$cursos <- if (nrow(dt) > 0) data.table::uniqueN(dt$curso_label) else 0L
    ser_diag_rv(diag)
    
    dt
  })
  
  output$ser_resumen_filtros <- shiny::renderUI({
    fa <- ser_filtros_aplicados()
    if (is.null(fa)) {
      return(shiny::helpText("Ajusta filtros y presiona 'Aplicar filtros' para calcular la serie."))
    }
    
    shiny::tagList(
      shiny::tags$b("Filtros actuales:"),
      shiny::tags$ul(
        shiny::tags$li(paste0("Años: ", paste(fa$rango_anos, collapse = "–"))),
        shiny::tags$li(paste0("Granularidad: ", fa$granularidad)),
        shiny::tags$li(paste0("Indicador: ", fa$indicador)),
        shiny::tags$li(paste0(
          "Modalidades: ",
          if (is.null(fa$modalidades) || length(fa$modalidades) == 0) "Todas" else paste(fa$modalidades, collapse = ", ")
        )),
        shiny::tags$li(paste0(
          "Periodo/ciclo: ",
          if (is.null(fa$periodos) || length(fa$periodos) == 0) "Todos" else paste(fa$periodos, collapse = ", ")
        )),
        shiny::tags$li(paste0(
          "Cursos: ",
          if (is.null(fa$cursos) || length(fa$cursos) == 0) "Todos" else paste(fa$cursos, collapse = ", ")
        ))
      )
    )
  })
  
  output$ser_diag_filtros <- shiny::renderUI({
    d <- ser_diag_rv()
    if (is.null(d)) return(NULL)
    shiny::tagList(
      shiny::tags$hr(),
      shiny::tags$div(
        style = "font-size: 12px;",
        shiny::tags$b("Diagnóstico: "),
        shiny::tags$span(
          paste0(
            "Base: ", format(d$base_rows, big.mark = ","),
            " | Filtrado: ", format(d$filtrado_rows, big.mark = ","),
            " | Estudiantes: ", format(d$estudiantes, big.mark = ","),
            " | Cursos: ", format(d$cursos, big.mark = ",")
          )
        )
      )
    )
  })
  
  ser_serie <- shiny::reactive({
    dt <- ser_datos_filtrados()
    fa <- ser_filtros_aplicados()
    if (is.null(fa) || is.null(dt) || nrow(dt) == 0) return(NULL)
    if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
    
    indicador <- fa$indicador
    granularidad <- fa$granularidad
    
    # ----------------------------
    # Eficiencia terminal (cohorte)
    # ----------------------------
    if (identical(indicador, "Eficiencia terminal (cohorte)")) {
      if (!identical(granularidad, "Cohorte")) {
        return(data.table::data.table(
          etiqueta = "Seleccione granularidad 'Cohorte' para este indicador.",
          valor = NA_real_
        ))
      }
      
      # Nivel estudiante: cohorte + estado + fecha de graduación
      est <- inst_estado_estudiante()
      if (!data.table::is.data.table(est)) est <- data.table::as.data.table(est)
      
      coh_map <- dt[!is.na(identificador) & !is.na(cohorte),
                    .(cohorte = min(cohorte, na.rm = TRUE)),
                    by = identificador
      ]
      
      est2 <- merge(coh_map, est, by = "identificador", all.x = TRUE)
      
      tiempo_std <- suppressWarnings(as.numeric(input$param_tiempo_estandar_anos))
      if (!is.finite(tiempo_std) || is.na(tiempo_std)) tiempo_std <- 6
      
      est2[, anio_graduacion := if (!is.na(graduacion_dt)) lubridate::year(graduacion_dt) else NA_integer_]
      est2[, en_tiempo := !is.na(anio_graduacion) & (anio_graduacion - cohorte) <= tiempo_std]
      
      ser <- est2[
        ,
        .(
          total = .N,
          graduados = sum(estado == "Graduado", na.rm = TRUE),
          graduados_en_tiempo = sum(en_tiempo, na.rm = TRUE),
          valor = if (.N > 0) round(100 * graduados_en_tiempo / .N, 2) else NA_real_
        ),
        by = cohorte
      ][order(cohorte)]
      
      ser[, etiqueta := as.character(cohorte)]
      ser[, indicador := "Eficiencia terminal (% graduados en tiempo)"]
      
      return(ser[, .(orden = cohorte, etiqueta, valor, total, graduados, graduados_en_tiempo, indicador)])
    }
    
    # ----------------------------
    # Deserción (t+1 por ciclo)
    # ----------------------------
    if (identical(indicador, "Deserción (t+1 por ciclo)")) {
      if (!("ciclo_id" %in% names(dt)) || !("periodo_orden" %in% names(dt))) return(NULL)
      
      # Excluir egresados (Graduado/Cierre) del denominador, como se definió
      est <- inst_estado_estudiante()
      if (!data.table::is.data.table(est)) est <- data.table::as.data.table(est)
      
      # Actividad por estudiante y ciclo (existencia de cursos)
      act <- unique(dt[!is.na(identificador) & !is.na(ciclo_id),
                       .(identificador, ano, periodo_orden, ciclo_id, ciclo_label)
      ])
      
      act <- merge(act, est[, .(identificador, estado)], by = "identificador", all.x = TRUE)
      act <- act[!estado %in% c("Graduado", "Cierre")]
      
      # Próximo ciclo "calendario" estimado a partir de año + orden
      act[, next_ciclo_id := data.table::fifelse(
        periodo_orden %in% 1:3,
        (ano * 10L) + (periodo_orden + 1L),
        ((ano + 1L) * 10L) + 1L
      )]
      
      # Marcar si el estudiante tuvo cursos en el ciclo siguiente
      act[, key_actual := paste0(identificador, "|", ciclo_id)]
      key_set <- unique(act$key_actual)
      act[, key_siguiente := paste0(identificador, "|", next_ciclo_id)]
      act[, desertor := !(key_siguiente %in% key_set)]
      
      ser_ciclo <- act[
        ,
        .(
          estudiantes = data.table::uniqueN(identificador),
          desertores = sum(desertor, na.rm = TRUE),
          valor = round(100 * sum(desertor, na.rm = TRUE) / data.table::uniqueN(identificador), 2)
        ),
        by = .(ciclo_id, ano, ciclo_label)
      ][order(ciclo_id)]
      
      if (identical(granularidad, "Ciclo/Periodo")) {
        ser <- ser_ciclo[, .(orden = ciclo_id, etiqueta = ciclo_label, valor, estudiantes, desertores)]
        ser[, indicador := "Deserción (% no reinscrito en t+1)"]
        return(ser)
      }
      
      if (identical(granularidad, "Año")) {
        ser <- ser_ciclo[
          ,
          .(
            estudiantes = sum(estudiantes, na.rm = TRUE),
            desertores = sum(desertores, na.rm = TRUE),
            valor = round(100 * sum(desertores, na.rm = TRUE) / sum(estudiantes, na.rm = TRUE), 2)
          ),
          by = ano
        ][order(ano)]
        ser[, `:=`(orden = ano, etiqueta = as.character(ano), indicador = "Deserción (% no reinscrito en t+1)")]
        return(ser[, .(orden, etiqueta, valor, estudiantes, desertores, indicador)])
      }
      
      return(data.table::data.table(
        orden = NA_real_,
        etiqueta = "Para 'Deserción', use granularidad 'Ciclo/Periodo' o 'Año'.",
        valor = NA_real_
      ))
    }
    
    # ----------------------------
    # Indicadores por promedio / aprobación / reprobación
    # ----------------------------
    dt2 <- dt[!is.na(nota)]
    
    if (identical(granularidad, "Año")) {
      ser <- dt2[
        ,
        .(
          estudiantes = data.table::uniqueN(identificador),
          cursos = data.table::uniqueN(curso_label),
          evaluaciones = .N,
          aprobados = sum(nota >= 61, na.rm = TRUE)
        ),
        by = ano
      ][order(ano)]
      ser[, `:=`(orden = ano, etiqueta = as.character(ano))]
    } else if (identical(granularidad, "Ciclo/Periodo")) {
      ser <- dt2[
        ,
        .(
          estudiantes = data.table::uniqueN(identificador),
          cursos = data.table::uniqueN(curso_label),
          evaluaciones = .N,
          aprobados = sum(nota >= 61, na.rm = TRUE)
        ),
        by = .(ciclo_id, ciclo_label)
      ][order(ciclo_id)]
      ser[, `:=`(orden = ciclo_id, etiqueta = ciclo_label)]
    } else if (identical(granularidad, "Cohorte")) {
      # Cohorte derivada del historial filtrado
      coh_map <- dt[!is.na(identificador) & !is.na(cohorte),
                    .(cohorte = min(cohorte, na.rm = TRUE)),
                    by = identificador
      ]
      dt3 <- merge(dt2, coh_map, by = "identificador", all.x = TRUE)
      ser <- dt3[
        ,
        .(
          estudiantes = data.table::uniqueN(identificador),
          cursos = data.table::uniqueN(curso_label),
          evaluaciones = .N,
          aprobados = sum(nota >= 61, na.rm = TRUE)
        ),
        by = cohorte
      ][order(cohorte)]
      ser[, `:=`(orden = cohorte, etiqueta = as.character(cohorte))]
    } else {
      return(NULL)
    }
    
    if (identical(indicador, "Promedio")) {
      dtmp <- dt2
      if (isTRUE(fa$promedio_solo_aprobados)) {
        dtmp <- dtmp[nota >= 61]
      }
      
      if (identical(granularidad, "Año")) {
        ser_val <- dtmp[, .(valor = round(mean(nota, na.rm = TRUE), 2)), by = ano]
        ser <- merge(ser, ser_val, by = "ano", all.x = TRUE)
      } else if (identical(granularidad, "Ciclo/Periodo")) {
        ser_val <- dtmp[, .(valor = round(mean(nota, na.rm = TRUE), 2)), by = .(ciclo_id, ciclo_label)]
        ser <- merge(ser, ser_val, by = c("ciclo_id", "ciclo_label"), all.x = TRUE)
      } else {
        coh_map <- dt[!is.na(identificador) & !is.na(cohorte),
                      .(cohorte = min(cohorte, na.rm = TRUE)),
                      by = identificador
        ]
        dtmp2 <- merge(dtmp, coh_map, by = "identificador", all.x = TRUE)
        ser_val <- dtmp2[, .(valor = round(mean(nota, na.rm = TRUE), 2)), by = cohorte]
        ser <- merge(ser, ser_val, by = "cohorte", all.x = TRUE)
      }
      
      ser[, indicador := if (isTRUE(fa$promedio_solo_aprobados)) "Promedio (solo aprobados)" else "Promedio (todos)"]
    } else if (identical(indicador, "% Aprobación")) {
      ser[, valor := round(100 * aprobados / evaluaciones, 2)]
      ser[, indicador := "% Aprobación"]
    } else if (identical(indicador, "% Reprobación")) {
      ser[, valor := round(100 * (evaluaciones - aprobados) / evaluaciones, 2)]
      ser[, indicador := "% Reprobación"]
    } else {
      ser[, valor := NA_real_]
      ser[, indicador := indicador]
    }
    
    # Suavizado (media móvil 3) sobre el valor
    if (isTRUE(fa$suavizado) && nrow(ser) >= 3) {
      ser[, valor_suav := as.numeric(stats::filter(valor, rep(1 / 3, 3), sides = 2))]
    } else {
      ser[, valor_suav := NA_real_]
    }
    
    ser
  })
  
  output$grafico_series <- plotly::renderPlotly({
    ser <- ser_serie()
    fa <- ser_filtros_aplicados()
    
    if (is.null(fa)) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "Ajusta filtros y presiona 'Aplicar filtros'."))
    }
    
    if (is.null(ser) || nrow(ser) == 0 || all(is.na(ser$valor))) {
      return(plotly::plot_ly() %>%
               plotly::layout(
                 title = "No hay datos para la combinación de filtros / indicador seleccionados.",
                 xaxis = list(title = ""),
                 yaxis = list(title = "")
               ))
    }
    
    y_col <- if (isTRUE(fa$suavizado) && "valor_suav" %in% names(ser) && any(!is.na(ser$valor_suav))) "valor_suav" else "valor"
    
    # Para Ciclo/Periodo, usamos un eje ordenado y etiquetas
    if (identical(fa$granularidad, "Ciclo/Periodo")) {
      x_vals <- seq_len(nrow(ser))
      ticktext <- ser$etiqueta
      tickvals <- x_vals
      
      p <- plotly::plot_ly(
        data = ser,
        x = x_vals,
        y = ser[[y_col]],
        type = "scatter",
        mode = "lines+markers",
        hoverinfo = "text",
        text = paste0(
          "<b>", ser$etiqueta, "</b>",
          "<br>Valor: ", ser[[y_col]]
        )
      ) %>%
        plotly::layout(
          title = paste0("Serie: ", fa$indicador, " (", fa$granularidad, ")"),
          xaxis = list(title = "Ciclo/Periodo", tickmode = "array", tickvals = tickvals, ticktext = ticktext),
          yaxis = list(title = "Valor"),
          margin = list(b = 120)
        )
      return(p)
    }
    
    p <- plotly::plot_ly(
      data = ser,
      x = ~orden,
      y = ser[[y_col]],
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = paste0(
        "<b>", ser$etiqueta, "</b>",
        "<br>Valor: ", ser[[y_col]]
      )
    ) %>%
      plotly::layout(
        title = paste0("Serie: ", fa$indicador, " (", fa$granularidad, ")"),
        xaxis = list(title = fa$granularidad),
        yaxis = list(title = "Valor")
      )
    
    p
  })
  
  output$tabla_series <- DT::renderDT({
    ser <- ser_serie()
    fa <- ser_filtros_aplicados()
    
    if (is.null(fa)) {
      return(DT::datatable(data.frame(Mensaje = "Ajusta filtros y presiona 'Aplicar filtros'."), options = list(pageLength = 5)))
    }
    
    if (is.null(ser) || nrow(ser) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros / indicador seleccionados."), options = list(pageLength = 5)))
    }
    
    # Mostrar columnas clave con nombres claros
    out <- ser
    
    # Si hay suavizado, mostramos ambos
    if ("valor_suav" %in% names(out) && any(!is.na(out$valor_suav))) {
      out[, valor_suav := round(valor_suav, 2)]
    } else if ("valor_suav" %in% names(out)) {
      out[, valor_suav := NULL]
    }
    
    # Ordenar por orden
    if ("orden" %in% names(out)) out <- out[order(out$orden)]
    
    DT::datatable(
      out,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}