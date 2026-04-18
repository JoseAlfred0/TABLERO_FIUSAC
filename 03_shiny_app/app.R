# ==============================================================================
# TABLERO DE EFICACIA ACADÉMICA - FIUSAC (Versión 7.0 - Producción)
# Arquitectura Modular Optimizada - R 4.5.1
#
# SECCIONES:
# 1. Análisis Individual: Perfil estudiante, historial académico
# 2. Análisis Institucional: Métricas globales, comparativas
# 3. Series de Tiempo: Evolución, tendencias, cohortes
# ==============================================================================

# ------------------------------------------------------------------------------
# CARGA DE CÓDIGO MODULAR
# ------------------------------------------------------------------------------
# Core
source(file.path("..", "R", "core", "logging.R"))
source(file.path("..", "R", "core", "notifications.R"))
source(file.path("..", "R", "core", "startup_checks.R"))

# Config
source(file.path("..", "R", "config", "paths.R"))

# Data
source(file.path("..", "R", "data", "cleaning.R"))
source(file.path("..", "R", "data", "loaders.R"))
source(file.path("..", "R", "data", "cache.R"))

# Modules
source(file.path("..", "R", "modules", "filtros", "ui.R"))
source(file.path("..", "R", "modules", "filtros", "server.R"))
source(file.path("..", "R", "modules", "individual", "ui.R"))
source(file.path("..", "R", "modules", "individual", "server.R"))
source(file.path("..", "R", "modules", "institucional", "ui.R"))
source(file.path("..", "R", "modules", "institucional", "server.R"))
source(file.path("..", "R", "modules", "series", "ui.R"))
source(file.path("..", "R", "modules", "series", "server.R"))

# ------------------------------------------------------------------------------
# CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  # Core Shiny
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  
  # Datos
  library(dplyr)
  library(tidyr)
  library(data.table)
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
  library(memoise)
  library(digest)
  
  # Exportación
  library(writexl)
})

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  skin = "blue",
  
  header = shinydashboard::dashboardHeader(
    title = shiny::tags$div(
      style = "display: flex; align-items: center;",
      shiny::tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Logo_USAC.svg/1200px-Logo_USAC.svg.png",
        height = "35px",
        style = "margin-right: 15px;"
      ),
      shiny::tags$div(
        shiny::tags$h4(
          "Tablero FIUSAC V7.0 - Análisis Académico",
          style = "margin: 0; font-weight: bold; color: #0056a6;"
        ),
        shiny::tags$p(
          "Análisis Individual, Institucional y Series de Tiempo",
          style = "margin: 0; font-size: 12px; color: #666;"
        )
      )
    ),
    titleWidth = 500
  ),
  
  sidebar = shinydashboard::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      id = "menu_principal",
      shinydashboard::menuItem("🏠 Inicio", tabName = "inicio", icon = shiny::icon("home")),
      shinydashboard::menuItem("👤 Análisis Individual", tabName = "individual", icon = shiny::icon("user-graduate")),
      shinydashboard::menuItem("🏫 Análisis Institucional", tabName = "institucional", icon = shiny::icon("university")),
      shinydashboard::menuItem("📈 Series de Tiempo", tabName = "series", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("📋 Datos", tabName = "datos", icon = shiny::icon("database")),
      shinydashboard::menuItem("📤 Exportar", tabName = "exportar", icon = shiny::icon("download"))
    ),
    
    shiny::hr(),
    
    shiny::div(
      style = "padding: 15px; background-color: #f8f9fa;",
      shiny::h5("ℹ️ Información del Sistema"),
      shiny::verbatimTextOutput("info_sistema"),
      shiny::hr(),
      shiny::actionButton(
        "optimizar_memoria",
        "Optimizar Memoria",
        icon = shiny::icon("memory"),
        class = "btn-sm btn-block",
        style = "margin-bottom: 10px;"
      )
    )
  ),
  
  body = shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste0(
        "body { font-family: 'Segoe UI', 'Helvetica Neue', sans-serif; font-size: 14px; }\n",
        ".content-wrapper, .right-side { background-color: #f5f7fa; }\n",
        ".small-box { border-radius: 6px; margin-bottom: 10px; }\n",
        ".box { border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 15px; }\n",
        ".dataTables_wrapper { font-size: 12px; }\n",
        "table.dataTable { width: 100% !important; }\n",
        "@media (max-width: 768px) {\n",
        "  .box { margin-bottom: 10px; }\n",
        "  .tab-content > .tab-pane { padding: 10px; }\n",
        "  .col-md-6, .col-md-4, .col-md-3, .col-md-8, .col-md-12 {\n",
        "    padding-left: 5px; padding-right: 5px; }\n",
        "}\n",
        ".btn-block { width: 100%; margin-bottom: 5px; }\n",
        ".shiny-spinner-output-container { text-align: center; padding: 20px; }\n"
      )))
    ),
    
    shinydashboard::tabItems(
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
      
      shinydashboard::tabItem(
        tabName = "individual",
        shiny::fluidRow(
          shiny::column(12, mod_filtros_ui("filtros_individual"))
        ),
        shiny::fluidRow(
          shiny::column(12, mod_individual_ui("individual"))
        )
      ),
      
      mod_institucional_ui(),
      mod_series_ui(),
      
      shinydashboard::tabItem(
        tabName = "datos",
        shiny::h2("📋 Datos Completos"),
        shiny::fluidRow(
          shiny::column(12, mod_filtros_ui("filtros_datos"))
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            DT::DTOutput("tabla_datos") %>%
              shinycssloaders::withSpinner(color = "#3498db")
          )
        )
      ),
      
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
            shiny::downloadButton(
              "exportar_csv",
              "CSV",
              class = "btn-primary btn-block",
              style = "margin-bottom: 10px;"
            ),
            shiny::downloadButton(
              "exportar_excel",
              "Excel",
              class = "btn-success btn-block"
            )
          ),
          shinydashboard::box(
            title = "📈 Exportar Datos Filtrados",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            shiny::h4("Datos con filtros aplicados"),
            shiny::p("Incluye solo los datos visibles actualmente"),
            shiny::br(),
            shiny::downloadButton(
              "exportar_filtrados",
              "CSV Filtrado",
              class = "btn-info btn-block",
              style = "margin-bottom: 10px;"
            ),
            shiny::downloadButton(
              "exportar_reporte",
              "Reporte Completo",
              class = "btn-warning btn-block"
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  datos_crudos <- shiny::reactiveVal(NULL)
  datos_activos <- shiny::reactiveVal(NULL)
  datos_complementarios_rv <- shiny::reactiveVal(NULL)
  
  shiny::observe({
    tryCatch({
      log_message("info", "Iniciando carga de datos...")
      
      archivo_historico <- resolver_ruta_datos()
      mtime_actual <- obtener_mtime_seguro(archivo_historico)
      
      datos <- obtener_desde_cache_mtime("datos", "datos_mtime", mtime_actual)
      if (is.null(datos)) {
        datos <- cargar_datos_memoised()
        guardar_en_cache_mtime("datos", "datos_mtime", datos, mtime_actual)
      }
      
      datos_crudos(datos)
      datos_activos(datos)
      
      archivo_comp <- resolver_ruta_complementarios()
      mtime_comp <- if (!is.null(archivo_comp)) obtener_mtime_seguro(archivo_comp) else NA
      
      comp <- NULL
      if (!is.null(archivo_comp)) {
        comp <- obtener_desde_cache_mtime("comp", "comp_mtime", mtime_comp)
        if (is.null(comp)) {
          comp <- cargar_complementarios_optimizado()
          guardar_en_cache_mtime("comp", "comp_mtime", comp, mtime_comp)
        }
      }
      
      datos_complementarios_rv(comp)
      notify_user("Datos cargados correctamente", tipo = "message", duracion = 5)
    }, error = function(e) {
      notify_user(paste("Error al cargar datos:", e$message), tipo = "error", duracion = 10)
    })
  })
  
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
  
  shiny::observeEvent(input$optimizar_memoria, {
    gc(verbose = FALSE, full = TRUE)
    notify_user("Memoria optimizada", tipo = "default", duracion = 3)
  })
  
  filtros_individual <- mod_filtros_server("filtros_individual", datos_crudos)
  filtros_datos <- mod_filtros_server("filtros_datos", datos_crudos)
  
  filtros <- shiny::reactive({
    if (is.null(input$menu_principal)) return(filtros_individual())
    if (identical(input$menu_principal, "datos")) return(filtros_datos())
    filtros_individual()
  })
  
  shiny::observe({
    shiny::req(datos_crudos(), filtros())
    
    tryCatch({
      datos <- data.table::as.data.table(datos_crudos())
      filtros_aplicar <- filtros()
      
      if ((is.null(filtros_aplicar$rango_anos) || length(filtros_aplicar$rango_anos) == 0) &&
          (is.null(filtros_aplicar$modalidad) || length(filtros_aplicar$modalidad) == 0) &&
          (is.null(filtros_aplicar$periodo) || length(filtros_aplicar$periodo) == 0) &&
          (is.null(filtros_aplicar$curso) || length(filtros_aplicar$curso) == 0) &&
          (is.null(filtros_aplicar$estudiante) || filtros_aplicar$estudiante == "")) {
        datos_activos(datos)
        return()
      }
      
      idx <- rep(TRUE, nrow(datos))
      
      if (isTRUE(identical(filtros_aplicar$seccion, "individual"))) {
        estudiante_id <- filtros_aplicar$estudiante
        id_manual <- filtros_aplicar$id_manual
        
        if (!is.null(estudiante_id) && estudiante_id != "" && "identificador" %in% names(datos)) {
          idx <- idx & (datos$identificador == estudiante_id)
        } else if (!is.null(id_manual) && id_manual != "" && "identificador" %in% names(datos)) {
          idx <- idx & (datos$identificador == id_manual)
        }
      } else {
        if (!is.null(filtros_aplicar$rango_anos) && length(filtros_aplicar$rango_anos) > 0) {
          anos_sel <- suppressWarnings(as.integer(filtros_aplicar$rango_anos))
          anos_sel <- anos_sel[!is.na(anos_sel)]
          if (length(anos_sel) > 0 && "ano" %in% names(datos)) {
            idx <- idx & (datos$ano %in% anos_sel)
          }
        }
        
        if (!is.null(filtros_aplicar$modalidad) && length(filtros_aplicar$modalidad) > 0 && "modalidad" %in% names(datos)) {
          idx <- idx & (datos$modalidad %in% filtros_aplicar$modalidad)
        }
        
        if (!is.null(filtros_aplicar$periodo) && length(filtros_aplicar$periodo) > 0 && "periodo" %in% names(datos)) {
          idx <- idx & (datos$periodo %in% filtros_aplicar$periodo)
        }
        
        if (!is.null(filtros_aplicar$curso) && length(filtros_aplicar$curso) > 0 && "curso" %in% names(datos)) {
          idx <- idx & (datos$curso %in% filtros_aplicar$curso)
        }
      }
      
      if (any(idx)) {
        datos_activos(datos[idx])
      } else {
        datos_activos(datos[0])
      }
    }, error = function(e) {
      notify_user(paste0("Error al aplicar filtros: ", e$message), tipo = "error", duracion = 8)
      datos_activos(datos_crudos())
    })
  })
  
  mod_individual_server(
    "individual",
    shiny::reactive({ datos_activos() }),
    datos_complementarios = shiny::reactive({ datos_complementarios_rv() }),
    filtros_modulo_id = "filtros_individual"
  )
  
  # --------------------------------------------------------------------------
  # BASE INSTITUCIONAL COMPARTIDA
  # --------------------------------------------------------------------------
  inst_diag_rv <- shiny::reactiveVal(NULL)
  
  inst_dt_base <- shiny::reactive({
    datos <- datos_activos()
    if (is.null(datos) || nrow(datos) == 0) return(NULL)
    if (!data.table::is.data.table(datos)) datos <- data.table::as.data.table(datos)
    
    cols_req <- c("identificador", "curso", "nombrecurso", "ano", "periodo", "nota", "modalidad")
    if (!all(cols_req %in% names(datos))) {
      datos <- janitor::clean_names(as.data.frame(datos))
      datos <- data.table::as.data.table(datos)
      if (!all(cols_req %in% names(datos))) return(NULL)
    }
    
    datos <- datos[!(
      identificador %in% c("identificador", "ID", "Id") |
        curso %in% c("codcurso", "curso") |
        is.na(ano)
    )]
    
    datos[, identificador := as.character(identificador)]
    datos[, curso := as.character(curso)]
    datos[, nombrecurso := as.character(nombrecurso)]
    datos[, periodo := stringr::str_squish(as.character(periodo))]
    datos[, modalidad := stringr::str_squish(as.character(modalidad))]
    datos[, ano := suppressWarnings(as.integer(ano))]
    datos[, nota := suppressWarnings(as.numeric(nota))]
    
    datos[, curso_label := data.table::fifelse(
      !is.na(nombrecurso) & nombrecurso != "",
      nombrecurso,
      curso
    )]
    
    datos[, aprobado := !is.na(nota) & nota >= 61]
    
    datos[, periodo_orden := dplyr::case_when(
      stringr::str_detect(stringr::str_to_upper(periodo), "PRIMER") ~ 1L,
      stringr::str_detect(stringr::str_to_upper(periodo), "VAC") &
        stringr::str_detect(stringr::str_to_upper(periodo), "JUN") ~ 2L,
      stringr::str_detect(stringr::str_to_upper(periodo), "SEGUNDO") ~ 3L,
      stringr::str_detect(stringr::str_to_upper(periodo), "VAC") &
        stringr::str_detect(stringr::str_to_upper(periodo), "DIC") ~ 4L,
      stringr::str_detect(stringr::str_to_upper(periodo), "VAC") ~ 2L,
      TRUE ~ 9L
    )]
    
    datos[, periodo_tipo := periodo]
    datos[, ciclo_id := (ano * 10L) + periodo_orden]
    datos[, ciclo_label := paste0(ano, " - ", periodo)]
    
    coh <- datos[!is.na(ano), .(cohorte = min(ano, na.rm = TRUE)), by = identificador]
    datos <- merge(datos, coh, by = "identificador", all.x = TRUE)
    
    data.table::setkey(datos, identificador)
    datos
  })
  
  mod_institucional_server(
    input = input,
    output = output,
    session = session,
    datos_activos = datos_activos,
    datos_complementarios_rv = datos_complementarios_rv,
    inst_dt_base = inst_dt_base
  )
  
  inst_estado_estudiante <- shiny::reactive({
    comp <- datos_complementarios_rv()
    if (is.null(comp) || nrow(comp) == 0) return(NULL)
    if (!data.table::is.data.table(comp)) comp <- data.table::as.data.table(comp)
    comp[, identificador := trimws(as.character(identificador))]
    
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
  
  mod_series_server(
    input = input,
    output = output,
    session = session,
    inst_dt_base = inst_dt_base,
    inst_estado_estudiante = inst_estado_estudiante
  )
  
  output$tabla_datos <- DT::renderDT({
    shiny::req(datos_activos())
    
    tryCatch({
      datos <- datos_activos()
      
      if (nrow(datos) > 1000) {
        datos <- datos[1:1000, ]
      }
      
      DT::datatable(
        datos,
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel"),
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
          ),
          scrollX = TRUE,
          scrollY = "400px",
          deferRender = TRUE,
          scroller = TRUE
        ),
        extensions = c("Buttons", "Scroller"),
        rownames = FALSE,
        filter = "top",
        class = "display compact stripe"
      )
    }, error = function(e) {
      notify_user(paste0("Error al generar tabla de datos: ", e$message), tipo = "error", duracion = 8)
      DT::datatable(data.frame(Mensaje = "No se pudo cargar la tabla."), options = list(dom = "t"))
    })
  })
  
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
      
      if (data.table::is.data.table(datos_activos())) {
        reporte <- datos_activos()[
          !is.na(nota),
          .(
            Estudiantes = uniqueN(identificador),
            Cursos = uniqueN(curso),
            Promedio = round(mean(nota, na.rm = TRUE), 2),
            Tasa_Aprobacion = paste0(round(mean(nota >= 61, na.rm = TRUE) * 100, 1), "%"),
            Registros = .N
          )
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
  
  session$onSessionEnded(function() {
    log_message("info", "Sesión finalizada - Limpiando recursos")
    try(gc(verbose = FALSE), silent = TRUE)
  })
}

# ------------------------------------------------------------------------------
# EJECUCIÓN
# ------------------------------------------------------------------------------
if (interactive()) {
  if (verificar_requisitos()) {
    log_message("titulo", "INICIANDO APLICACIÓN SHINY")
    log_message("info", "Presione Ctrl+C para detener la aplicación")
    shiny::shinyApp(ui = ui, server = server)
  }
} else {
  shiny::shinyApp(ui = ui, server = server)
}