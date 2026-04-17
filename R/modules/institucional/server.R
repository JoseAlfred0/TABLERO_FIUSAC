# R/modules/institucional/server.R
mod_institucional_server <- function(input, output, session, datos_activos, datos_complementarios_rv, inst_dt_base) {
  # Poblar choices de filtros cuando hay datos
  shiny::observeEvent(inst_dt_base(), {
    dt <- inst_dt_base()
    if (is.null(dt) || nrow(dt) == 0) return()
    
    anios <- sort(unique(dt$ano))
    if (length(anios) > 0) {
      shiny::updateSliderInput(session, "inst_rango_anos",
                               min = min(anios, na.rm = TRUE),
                               max = max(anios, na.rm = TRUE),
                               value = c(min(anios, na.rm = TRUE), max(anios, na.rm = TRUE)))
    }
    
    shiny::updateSelectizeInput(session, "inst_modalidades", choices = sort(unique(dt$modalidad)), selected = NULL, server = TRUE)
    shiny::updateSelectizeInput(session, "inst_periodos", choices = sort(unique(dt$periodo)), selected = NULL, server = TRUE)
    shiny::updateSelectizeInput(session, "inst_cursos", choices = sort(unique(dt$curso_label)), selected = NULL, server = TRUE)
    shiny::updateSelectizeInput(session, "inst_cohortes", choices = sort(unique(dt$cohorte)), selected = NULL, server = TRUE)
    
    comp <- datos_complementarios_rv()
    if (!is.null(comp) && nrow(comp) > 0) {
      if (!data.table::is.data.table(comp)) comp <- data.table::as.data.table(comp)
      if ("tipo_pensum" %in% names(comp)) shiny::updateSelectizeInput(session, "inst_tipo_pensum", choices = sort(unique(comp$tipo_pensum)), selected = NULL, server = TRUE)
      if ("carrera" %in% names(comp)) shiny::updateSelectizeInput(session, "inst_carreras", choices = sort(unique(comp$carrera)), selected = NULL, server = TRUE)
      if ("departamento" %in% names(comp)) shiny::updateSelectizeInput(session, "inst_departamentos", choices = sort(unique(comp$departamento)), selected = NULL, server = TRUE)
      if ("municipio" %in% names(comp)) shiny::updateSelectizeInput(session, "inst_municipios", choices = sort(unique(comp$municipio)), selected = NULL, server = TRUE)
    }
  }, ignoreInit = FALSE)
  
  # Cascada departamento -> municipio
  shiny::observeEvent(input$inst_departamentos, {
    comp <- datos_complementarios_rv()
    if (is.null(comp) || nrow(comp) == 0) return()
    if (!data.table::is.data.table(comp)) comp <- data.table::as.data.table(comp)
    if (!all(c("departamento","municipio") %in% names(comp))) return()
    
    if (is.null(input$inst_departamentos) || length(input$inst_departamentos) == 0) {
      shiny::updateSelectizeInput(session, "inst_municipios", choices = sort(unique(comp$municipio)), selected = NULL, server = TRUE)
    } else {
      muni <- sort(unique(comp[departamento %in% input$inst_departamentos, municipio]))
      shiny::updateSelectizeInput(session, "inst_municipios", choices = muni, selected = NULL, server = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Aplicación de filtros (NO recalcula hasta presionar el botón)
  inst_datos_filtrados <- shiny::eventReactive(input$inst_aplicar_filtros, {
    dt <- inst_dt_base()
    if (is.null(dt) || nrow(dt) == 0) return(NULL)
    
    # Asegurar tipos y normalización mínima (evita joins fallidos / espacios)
    if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
    if ("identificador" %in% names(dt)) dt[, identificador := trimws(as.character(identificador))]
    
    # Diagnóstico de filtrado (conteos por etapa)
    inst_diag <- list()
    inst_diag$n_base <- nrow(dt)
    
    # Filtro por rango de años
    if (!is.null(input$inst_rango_anos) && length(input$inst_rango_anos) == 2) {
      dt <- dt[ano >= input$inst_rango_anos[1] & ano <= input$inst_rango_anos[2]]
    }
    inst_diag$n_anios <- nrow(dt)
    
    # Filtros por selección (si hay)
    if (!is.null(input$inst_modalidades) && length(input$inst_modalidades) > 0) {
      dt <- dt[modalidad %in% input$inst_modalidades]
    }
    inst_diag$n_modalidades <- nrow(dt)
    if (!is.null(input$inst_periodos) && length(input$inst_periodos) > 0) {
      dt <- dt[periodo %in% input$inst_periodos]
    }
    inst_diag$n_periodos <- nrow(dt)
    if (!is.null(input$inst_cursos) && length(input$inst_cursos) > 0) {
      dt <- dt[curso_label %in% input$inst_cursos]
    }
    inst_diag$n_cursos <- nrow(dt)
    if (!is.null(input$inst_cohortes) && length(input$inst_cohortes) > 0) {
      dt <- dt[cohorte %in% as.integer(input$inst_cohortes)]
    }
    inst_diag$n_cohortes <- nrow(dt)
    
    # Unir complementarios para filtros institucionales (departamento/municipio/pensum/carrera)
    comp <- datos_complementarios_rv()
    if (!is.null(comp) && nrow(comp) > 0) {
      if (!data.table::is.data.table(comp)) comp <- data.table::as.data.table(comp)
      comp[, identificador := trimws(as.character(identificador))]
      
      # Evitar joins cartesianos: asegurar 1 fila por identificador (selección determinística)
      cols_pref <- intersect(c("carrera", "tipo_pensum", "departamento", "municipio", "pensum"), names(comp))
      if (nrow(comp) > 0) {
        comp[, score_fila := 0L]
        if (length(cols_pref) > 0) {
          for (cl in cols_pref) {
            comp[!is.na(get(cl)) & trimws(as.character(get(cl))) != "", score_fila := score_fila + 1L]
          }
          data.table::setorder(comp, identificador, -score_fila)
          comp <- comp[, .SD[1], by = identificador]
        } else {
          comp <- unique(comp, by = "identificador")
        }
        comp[, score_fila := NULL]
      }
      
      data.table::setkey(comp, identificador)
      # Left join: conservar nrow(dt) sin duplicar registros
      dt <- comp[dt, on = "identificador"]
      
      inst_diag$n_join_comp <- nrow(dt)
      
      
      if ("tipo_pensum" %in% names(dt) && !is.null(input$inst_tipo_pensum) && length(input$inst_tipo_pensum) > 0) {
        dt <- dt[tipo_pensum %in% input$inst_tipo_pensum]
      }
      inst_diag$n_tipo_pensum <- nrow(dt)
      
      if ("carrera" %in% names(dt) && !is.null(input$inst_carreras) && length(input$inst_carreras) > 0) {
        dt <- dt[carrera %in% input$inst_carreras]
      }
      inst_diag$n_carreras <- nrow(dt)
      
      if ("departamento" %in% names(dt) && !is.null(input$inst_departamentos) && length(input$inst_departamentos) > 0) {
        dt <- dt[departamento %in% input$inst_departamentos]
      }
      inst_diag$n_departamentos <- nrow(dt)
      
      if ("municipio" %in% names(dt) && !is.null(input$inst_municipios) && length(input$inst_municipios) > 0) {
        dt <- dt[municipio %in% input$inst_municipios]
      }
      inst_diag$n_municipios <- nrow(dt)
    }
    
    # Solo aprobados (>=61)
    if (isTRUE(input$inst_solo_aprobados)) {
      dt <- dt[aprobado == TRUE]
    }
    
    # Final: guardar diagnóstico y devolver
    inst_diag$n_final <- nrow(dt)
    inst_diag$n_estudiantes_final <- if (nrow(dt) > 0) data.table::uniqueN(dt$identificador) else 0L
    inst_diag_rv(inst_diag)
    
    dt
  }, ignoreInit = FALSE)
  
  # Estado académico institucional (Graduado / Cierre / En proceso)
  inst_estado_estudiante <- shiny::reactive({
    comp <- datos_complementarios_rv()
    if (is.null(comp) || nrow(comp) == 0) return(NULL)
    if (!data.table::is.data.table(comp)) comp <- data.table::as.data.table(comp)
    comp[, identificador := trimws(as.character(identificador))]
    
    # Parseo de fechas (si vienen como "YYYY-MM-DD")
    if ("graduacion" %in% names(comp)) comp[, graduacion_dt := suppressWarnings(lubridate::ymd(graduacion))]
    if ("cierre" %in% names(comp)) comp[, cierre_dt := suppressWarnings(lubridate::ymd(cierre))]
    if ("inscrito" %in% names(comp)) comp[, inscrito_dt := suppressWarnings(lubridate::ymd(inscrito))]
    
    comp[, estado := dplyr::case_when(
      !is.na(graduacion_dt) ~ "Graduado",
      is.na(graduacion_dt) & !is.na(cierre_dt) ~ "Cierre",
      TRUE ~ "En proceso"
    )]
    
    data.table::setkey(comp, identificador)
    comp
  })
  
  # Resumen de filtros aplicado
  output$inst_resumen_filtros <- shiny::renderUI({
    dt <- inst_datos_filtrados()
    if (is.null(dt) || nrow(dt) == 0) {
      return(shiny::tags$div(style="padding-top: 6px;",
                             shiny::tags$b("Filtros actuales:"),
                             shiny::tags$p("No hay datos (ajusta filtros y presiona 'Aplicar filtros').")))
    }
    anio_min <- suppressWarnings(min(dt$ano, na.rm = TRUE))
    anio_max <- suppressWarnings(max(dt$ano, na.rm = TRUE))
    mods <- if ("modalidad" %in% names(dt)) paste(sort(unique(dt$modalidad)), collapse = ", ") else "No disponible"
    shiny::tags$div(
      style="padding-top: 6px;",
      shiny::tags$b("Filtros actuales:"),
      shiny::tags$p(paste0("Años: ", anio_min, "–", anio_max)),
      shiny::tags$p(paste0("Modalidades: ", ifelse(nchar(mods)==0, "Todas", mods))),
      shiny::tags$p(paste0("Parámetro abandono: ", input$param_abandono_anos, " año(s); ",
                           "Tiempo estándar: ", input$param_tiempo_estandar_anos, " año(s). ",
                           "Si cambias parámetros, presiona 'Aplicar filtros'." ))
    )
  })
  
  
  # Diagnóstico de filtros (conteos por etapa) - útil cuando "No hay datos"
  output$inst_diag_filtros <- shiny::renderUI({
    diag <- inst_diag_rv()
    if (is.null(diag)) return(NULL)
    
    getn <- function(x) if (is.null(x)) NA else x
    
    # Si hay datos, no mostrar diagnóstico
    if (!is.null(diag$n_final) && diag$n_final > 0) return(NULL)
    
    stages <- data.frame(
      etapa = c("Base", "Años", "Modalidades", "Períodos", "Cursos", "Cohortes",
                "Join complementarios", "Tipo pensum", "Carrera", "Departamento", "Municipio", "Final"),
      n_registros = c(getn(diag$n_base), getn(diag$n_anios), getn(diag$n_modalidades), getn(diag$n_periodos),
                      getn(diag$n_cursos), getn(diag$n_cohortes), getn(diag$n_join_comp), getn(diag$n_tipo_pensum),
                      getn(diag$n_carreras), getn(diag$n_departamentos), getn(diag$n_municipios), getn(diag$n_final))
    )
    
    # Detectar en qué etapa se volvió 0 (la primera que llegue a 0 después de Base)
    etapa_cero <- NA_character_
    for (i in 2:nrow(stages)) {
      if (!is.na(stages$n_registros[i]) && stages$n_registros[i] == 0) {
        etapa_cero <- stages$etapa[i]
        break
      }
    }
    
    sugerencias <- shiny::tags$ul(
      shiny::tags$li("Quita filtros muy restrictivos (por ejemplo: Curso, Carrera, Tipo de pensum o Período)."),
      shiny::tags$li("Amplía el rango de años."),
      shiny::tags$li("Prueba con Modalidades = Todas y Períodos = Todos para validar que el dataset responde.")
    )
    
    shiny::tags$div(
      style = "margin-top:10px; padding:10px; border:1px solid #f5c6cb; background:#f8d7da; color:#721c24; border-radius:6px;",
      shiny::tags$b("Diagnóstico: no se encontraron registros con los filtros seleccionados."),
      if (!is.na(etapa_cero)) shiny::tags$p(paste0("La primera etapa que dejó el resultado en 0 fue: ", etapa_cero, ".")) else NULL,
      shiny::tags$p("Conteo de registros por etapa:"),
      shiny::tags$ul(lapply(seq_len(nrow(stages)), function(i) shiny::tags$li(paste0(stages$etapa[i], ": ", stages$n_registros[i])))),
      shiny::tags$p("Sugerencias rápidas:"),
      sugerencias
    )
  })
  
  
  # Cálculos institucionales (se recalculan solo cuando inst_datos_filtrados cambia)
  inst_calculos <- shiny::reactive({
    dt <- inst_datos_filtrados()
    if (is.null(dt) || nrow(dt) == 0) return(NULL)
    if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
    
    # Conjunto de estudiantes en el filtro actual
    estudiantes <- unique(dt$identificador)
    n_est <- length(estudiantes)
    
    # Cursos distintos
    n_cur <- data.table::uniqueN(dt$curso_label)
    
    # Aprobación/Reprobación global (evaluados = nota no NA)
    evaluados <- sum(!is.na(dt$nota))
    aprobados <- sum(dt$aprobado == TRUE, na.rm = TRUE)
    tasa_aprob <- if (evaluados > 0) round(100 * aprobados / evaluados, 2) else NA_real_
    tasa_reprob <- if (!is.na(tasa_aprob)) round(100 - tasa_aprob, 2) else NA_real_
    
    # Estado y tiempos (si comp existe)
    comp_estado <- inst_estado_estudiante()
    graduados_n <- 0; cierres_n <- 0
    prom_t_grad <- NA_real_; prom_t_cierre <- NA_real_
    if (!is.null(comp_estado) && nrow(comp_estado) > 0) {
      comp_f <- comp_estado[identificador %in% estudiantes]
      graduados_n <- sum(comp_f$estado == "Graduado", na.rm = TRUE)
      cierres_n <- sum(comp_f$estado == "Cierre", na.rm = TRUE)
      
      # tiempos (años) desde inscrito
      if (all(c("inscrito_dt","graduacion_dt") %in% names(comp_f))) {
        aux <- comp_f[!is.na(inscrito_dt) & !is.na(graduacion_dt)]
        if (nrow(aux) > 0) prom_t_grad <- round(mean(as.numeric(difftime(aux$graduacion_dt, aux$inscrito_dt, units = "days"))/365.25, na.rm = TRUE), 2)
      }
      if (all(c("inscrito_dt","cierre_dt") %in% names(comp_f))) {
        aux <- comp_f[!is.na(inscrito_dt) & !is.na(cierre_dt)]
        if (nrow(aux) > 0) prom_t_cierre <- round(mean(as.numeric(difftime(aux$cierre_dt, aux$inscrito_dt, units = "days"))/365.25, na.rm = TRUE), 2)
      }
    }
    
    # Abandono: corte respecto al año final filtrado, excluyendo Graduado/Cierre
    anio_final <- if (!is.null(input$inst_rango_anos) && length(input$inst_rango_anos)==2) input$inst_rango_anos[2] else suppressWarnings(max(dt$ano, na.rm = TRUE))
    umbral <- anio_final - as.integer(input$param_abandono_anos)
    ultimo_anio <- dt[, .(ultimo_ano = max(ano, na.rm = TRUE)), by = identificador]
    ultimo_anio <- ultimo_anio[identificador %in% estudiantes]
    
    # excluir graduado/cierre del abandono
    if (!is.null(comp_estado) && nrow(comp_estado) > 0) {
      comp_f2 <- comp_estado[identificador %in% estudiantes, .(identificador, estado)]
      ultimo_anio <- merge(ultimo_anio, comp_f2, by = "identificador", all.x = TRUE)
      ultimo_anio[is.na(estado), estado := "En proceso"]
    } else {
      ultimo_anio[, estado := "En proceso"]
    }
    
    candidatos_abandono <- ultimo_anio[estado == "En proceso"]
    abandonados_n <- sum(candidatos_abandono$ultimo_ano <= umbral, na.rm = TRUE)
    denom_aband <- nrow(candidatos_abandono)
    tasa_aband <- if (denom_aband > 0) round(100 * abandonados_n / denom_aband, 2) else NA_real_
    
    # Aprobación/Reprobación por año
    anual <- dt[!is.na(nota), .(
      evaluados = .N,
      aprobados = sum(aprobado == TRUE, na.rm = TRUE)
    ), by = ano][order(ano)]
    anual[, tasa_aprob := round(100 * aprobados / evaluados, 2)]
    anual[, tasa_reprob := round(100 - tasa_aprob, 2)]
    
    # Aprobación/Reprobación por ciclo
    ciclo <- dt[!is.na(nota), .(
      evaluados = .N,
      aprobados = sum(aprobado == TRUE, na.rm = TRUE)
    ), by = .(ciclo_id, ciclo_label)][order(ciclo_id)]
    ciclo[, tasa_aprob := round(100 * aprobados / evaluados, 2)]
    ciclo[, tasa_reprob := round(100 - tasa_aprob, 2)]
    
    # Deserción por ciclo (t+1 por ciclo), excluyendo egresados
    # Denominador: estudiantes activos en ciclo t (excluyendo graduado/cierre)
    comp_f3 <- NULL
    if (!is.null(comp_estado) && nrow(comp_estado) > 0) comp_f3 <- comp_estado[identificador %in% estudiantes, .(identificador, estado)]
    
    ciclos_orden <- ciclo[, .(ciclo_id, ciclo_label)][order(ciclo_id)]
    deser <- data.table::data.table()
    if (nrow(ciclos_orden) >= 2) {
      # set de estudiantes por ciclo
      st_por_ciclo <- dt[, .(estudiantes = list(unique(identificador))), by = .(ciclo_id, ciclo_label)][order(ciclo_id)]
      for (i in 1:(nrow(st_por_ciclo)-1)) {
        c0 <- st_por_ciclo$ciclo_id[i]
        c1 <- st_por_ciclo$ciclo_id[i+1]
        lab0 <- st_por_ciclo$ciclo_label[i]
        lab1 <- st_por_ciclo$ciclo_label[i+1]
        
        s0 <- st_por_ciclo$estudiantes[[i]]
        s1 <- st_por_ciclo$estudiantes[[i+1]]
        
        # excluir egresados del denominador
        if (!is.null(comp_f3)) {
          eg <- comp_f3[identificador %in% s0 & estado %in% c("Graduado","Cierre"), identificador]
          s0_eff <- setdiff(s0, eg)
        } else {
          s0_eff <- s0
        }
        
        desertores <- setdiff(s0_eff, s1)
        denom <- length(s0_eff)
        tasa <- if (denom > 0) round(100 * length(desertores) / denom, 2) else NA_real_
        
        deser <- rbind(deser, data.table::data.table(
          ciclo_t = lab0,
          ciclo_t1 = lab1,
          activos_t = denom,
          desertores = length(desertores),
          tasa_desercion = tasa
        ), fill = TRUE)
      }
    }
    
    # Heatmap aprobación (Top 30 cursos por frecuencia)
    topc <- dt[, .N, by = curso_label][order(-N)][1:min(30, data.table::uniqueN(dt$curso_label))]
    dt_h <- dt[curso_label %in% topc$curso_label & !is.na(nota)]
    heat <- dt_h[, .(
      evaluados = .N,
      aprobados = sum(aprobado == TRUE, na.rm = TRUE)
    ), by = .(curso_label, ano)]
    heat[, tasa_aprob := round(100 * aprobados / evaluados, 2)]
    
    # Cohortes y eficiencia terminal (parámetro tiempo estándar)
    cohorte_tab <- dt[, .(cohorte = unique(cohorte)), by = identificador]
    cohorte_tab <- cohorte_tab[identificador %in% estudiantes]
    
    coh_summary <- cohorte_tab[, .(tam_cohorte = .N), by = cohorte][order(cohorte)]
    # Inicializar en 0 para evitar merges con sufijos (.x/.y)
    coh_summary[, graduados_en_tiempo := 0L]
    coh_summary[, eficiencia_terminal := NA_real_]
    
    if (!is.null(comp_estado) && nrow(comp_estado) > 0) {
      comp_f4 <- comp_estado[identificador %in% estudiantes]
      # año de graduación
      if ("graduacion_dt" %in% names(comp_f4)) comp_f4[, grad_year := lubridate::year(graduacion_dt)]
      comp_f4 <- comp_f4[estado == "Graduado" & !is.na(grad_year)]
      
      # unir cohorte
      comp_f4 <- merge(comp_f4, cohorte_tab, by = "identificador", all.x = TRUE)
      t_std <- as.integer(input$param_tiempo_estandar_anos)
      comp_f4[, en_tiempo := !is.na(cohorte) & grad_year <= (cohorte + t_std)]
      
      grad_tiempo <- comp_f4[en_tiempo == TRUE, .(graduados_en_tiempo = data.table::uniqueN(identificador)), by = cohorte]
      # Update-join (sin crear graduados_en_tiempo.x / .y)
      data.table::setkey(coh_summary, cohorte)
      data.table::setkey(grad_tiempo, cohorte)
      coh_summary[grad_tiempo, graduados_en_tiempo := i.graduados_en_tiempo]
      coh_summary[is.na(graduados_en_tiempo), graduados_en_tiempo := 0L]
      coh_summary[, eficiencia_terminal := round(100 * graduados_en_tiempo / tam_cohorte, 2)]
    }
    
    list(
      kpis = list(
        estudiantes = n_est,
        cursos = n_cur,
        tasa_aprob = tasa_aprob,
        tasa_reprob = tasa_reprob,
        graduados = graduados_n,
        cierres = cierres_n,
        prom_t_grad = prom_t_grad,
        prom_t_cierre = prom_t_cierre,
        abandonados = abandonados_n,
        tasa_aband = tasa_aband,
        anio_final = anio_final
      ),
      anual = anual,
      ciclo = ciclo,
      desercion = deser,
      heat = heat,
      cohortes = coh_summary,
      detalle = dt
    )
  })
  
  # KPIs (valueBoxes)
  output$kpi_inst_estudiantes <- shinydashboard::renderValueBox({
    res <- inst_calculos()
    if (is.null(res)) return(shinydashboard::valueBox("0", "Estudiantes", icon = shiny::icon("users"), color = "light-blue"))
    shinydashboard::valueBox(res$kpis$estudiantes, "Estudiantes", icon = shiny::icon("users"), color = "light-blue")
  })
  
  output$kpi_inst_cursos <- shinydashboard::renderValueBox({
    res <- inst_calculos()
    if (is.null(res)) return(shinydashboard::valueBox("0", "Cursos", icon = shiny::icon("book"), color = "aqua"))
    shinydashboard::valueBox(res$kpis$cursos, "Cursos", icon = shiny::icon("book"), color = "aqua")
  })
  
  output$kpi_inst_aprobacion <- shinydashboard::renderValueBox({
    res <- inst_calculos()
    if (is.null(res) || is.na(res$kpis$tasa_aprob)) return(shinydashboard::valueBox("NA", "% Aprobación", icon = shiny::icon("check-circle"), color = "green"))
    shinydashboard::valueBox(paste0(res$kpis$tasa_aprob, "%"), "% Aprobación", icon = shiny::icon("check-circle"), color = "green")
  })
  
  output$kpi_inst_reprobacion <- shinydashboard::renderValueBox({
    res <- inst_calculos()
    if (is.null(res) || is.na(res$kpis$tasa_reprob)) return(shinydashboard::valueBox("NA", "% Reprobación", icon = shiny::icon("times-circle"), color = "red"))
    shinydashboard::valueBox(paste0(res$kpis$tasa_reprob, "%"), "% Reprobación", icon = shiny::icon("times-circle"), color = "red")
  })
  
  output$kpi_inst_graduados <- shinydashboard::renderValueBox({
    res <- inst_calculos()
    if (is.null(res)) return(shinydashboard::valueBox("0", "Graduados (en filtro)", icon = shiny::icon("user-graduate"), color = "purple"))
    shinydashboard::valueBox(res$kpis$graduados, "Graduados (en filtro)", icon = shiny::icon("user-graduate"), color = "purple")
  })
  
  output$kpi_inst_abandono <- shinydashboard::renderValueBox({
    res <- inst_calculos()
    if (is.null(res) || is.na(res$kpis$tasa_aband)) return(shinydashboard::valueBox("NA", "% Abandono", icon = shiny::icon("user-times"), color = "yellow"))
    shinydashboard::valueBox(paste0(res$kpis$tasa_aband, "%"), "% Abandono (En proceso)", icon = shiny::icon("user-times"), color = "yellow")
  })
  
  # Gráficas institucionales
  output$inst_g_aprob_reprob_anual <- plotly::renderPlotly({
    res <- inst_calculos()
    if (is.null(res)) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    df <- res$anual
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    
    plotly::plot_ly(df, x = ~ano) %>%
      plotly::add_lines(y = ~tasa_aprob, name = "Aprobación (%)") %>%
      plotly::add_lines(y = ~tasa_reprob, name = "Reprobación (%)") %>%
      plotly::layout(title = "Aprobación vs Reprobación por Año",
                     xaxis = list(title = "Año"),
                     yaxis = list(title = "Porcentaje", range = c(0, 100)))
  })
  
  output$inst_g_aprob_reprob_ciclo <- plotly::renderPlotly({
    res <- inst_calculos()
    if (is.null(res)) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    df <- res$ciclo
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    
    plotly::plot_ly(df, x = ~ciclo_label) %>%
      plotly::add_lines(y = ~tasa_aprob, name = "Aprobación (%)") %>%
      plotly::add_lines(y = ~tasa_reprob, name = "Reprobación (%)") %>%
      plotly::layout(title = "Aprobación vs Reprobación por Ciclo",
                     xaxis = list(title = "Ciclo", tickangle = -45),
                     yaxis = list(title = "Porcentaje", range = c(0, 100)))
  })
  
  output$inst_g_desercion_ciclo <- plotly::renderPlotly({
    res <- inst_calculos()
    if (is.null(res)) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    df <- res$desercion
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay deserción calculable (se requieren ≥2 ciclos)"))
    
    plotly::plot_ly(df, x = ~ciclo_t, y = ~tasa_desercion, type = "bar") %>%
      plotly::layout(title = "Deserción por Ciclo (t+1 definido por ciclo)",
                     xaxis = list(title = "Ciclo t", tickangle = -45),
                     yaxis = list(title = "Deserción (%)", range = c(0, 100)))
  })
  
  output$inst_tabla_desercion <- DT::renderDT({
    res <- inst_calculos()
    if (is.null(res)) return(DT::datatable(data.frame(Mensaje = "No hay datos"), options = list(pageLength = 5), rownames = FALSE))
    df <- res$desercion
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(Mensaje = "No hay deserción calculable (se requieren ≥2 ciclos)"),
                                                           options = list(pageLength = 5), rownames = FALSE))
    DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$inst_heatmap_aprob <- plotly::renderPlotly({
    res <- inst_calculos()
    if (is.null(res)) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    df <- res$heat
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos suficientes para heatmap"))
    
    # matriz para plotly heatmap
    cursos <- sort(unique(df$curso_label))
    anios <- sort(unique(df$ano))
    mat <- matrix(NA_real_, nrow = length(cursos), ncol = length(anios),
                  dimnames = list(cursos, anios))
    for (i in seq_len(nrow(df))) {
      mat[as.character(df$curso_label[i]), as.character(df$ano[i])] <- df$tasa_aprob[i]
    }
    
    plotly::plot_ly(
      x = anios, y = cursos, z = mat,
      type = "heatmap"
    ) %>%
      plotly::layout(title = "Heatmap: Tasa de aprobación (Top 30 cursos por frecuencia)",
                     xaxis = list(title = "Año"),
                     yaxis = list(title = "Curso"))
  })
  
  output$inst_g_cohortes <- plotly::renderPlotly({
    res <- inst_calculos()
    if (is.null(res)) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos"))
    df <- res$cohortes
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos de cohortes"))
    
    plotly::plot_ly(df, x = ~cohorte) %>%
      plotly::add_bars(y = ~tam_cohorte, name = "Tamaño cohorte") %>%
      plotly::add_bars(y = ~graduados_en_tiempo, name = "Graduados en tiempo") %>%
      plotly::layout(barmode = "group",
                     title = "Cohortes: tamaño vs graduados en tiempo (según parámetro)",
                     xaxis = list(title = "Cohorte"),
                     yaxis = list(title = "Conteo"))
  })
  
  output$inst_tabla_cohortes <- DT::renderDT({
    res <- inst_calculos()
    if (is.null(res)) return(DT::datatable(data.frame(Mensaje = "No hay datos"), options = list(pageLength = 5), rownames = FALSE))
    df <- res$cohortes
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(Mensaje = "No hay datos de cohortes"),
                                                           options = list(pageLength = 5), rownames = FALSE))
    DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$inst_tabla_detalle <- DT::renderDT({
    res <- inst_calculos()
    if (is.null(res)) return(DT::datatable(data.frame(Mensaje = "No hay datos"), options = list(pageLength = 5), rownames = FALSE))
    dt <- res$detalle
    if (is.null(dt) || nrow(dt) == 0) return(DT::datatable(data.frame(Mensaje = "No hay datos"), options = list(pageLength = 5), rownames = FALSE))
    # limitar filas por rendimiento
    df <- as.data.frame(dt)
    if (nrow(df) > 20000) df <- df[1:20000, ]
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$grafico_top_cursos <- plotly::renderPlotly({
    datos <- datos_activos()
    if (is.null(datos) || nrow(datos) == 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos disponibles"))
    }
    
    # Normalizar a data.table
    if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)
    
    top_cursos <- datos[!is.na(nota) & !is.na(curso),
                        .(promedio = mean(nota, na.rm = TRUE)),
                        by = curso][order(-promedio)][1:10]
    
    if (nrow(top_cursos) == 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = "No hay datos disponibles"))
    }
    
    plotly::plot_ly(
      top_cursos,
      x = ~reorder(curso, promedio),
      y = ~promedio,
      type = "bar"
    ) %>%
      plotly::layout(
        title = "Top 10 Cursos por Promedio",
        xaxis = list(title = "Curso"),
        yaxis = list(title = "Promedio", range = c(0, 100))
      )
  })
  
  output$tabla_estadisticas_ano <- DT::renderDT({
    datos <- datos_activos()
    if (is.null(datos) || nrow(datos) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos disponibles"), options = list(dom = "t")))
    }
    
    if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)
    
    stats_ano <- datos[!is.na(nota) & !is.na(ano),
                       .(Estudiantes = data.table::uniqueN(identificador),
                         Cursos = data.table::uniqueN(curso),
                         Promedio = round(mean(nota, na.rm = TRUE), 2),
                         Aprobacion = paste0(round(mean(nota >= 61, na.rm = TRUE) * 100, 1), "%")),
                       by = ano][order(ano)]
    
    DT::datatable(stats_ano, options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$tabla_institucional <- DT::renderDT({
    datos <- datos_activos()
    if (is.null(datos) || nrow(datos) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos disponibles"), options = list(dom = "t")))
    }
    
    DT::datatable(datos,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
}