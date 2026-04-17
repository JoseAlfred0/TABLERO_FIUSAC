# R/modules/individual/ui.R
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