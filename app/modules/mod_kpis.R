# Módulo KPIs (shinydashboard)

mod_kpis_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(2, shinydashboard::valueBoxOutput(ns("kpi_aprob"), width = 12)),
    column(2, shinydashboard::valueBoxOutput(ns("kpi_reprob"), width = 12)),
    column(2, shinydashboard::valueBoxOutput(ns("kpi_retiro"), width = 12)),
    column(3, shinydashboard::valueBoxOutput(ns("kpi_rend"), width = 12)),
    column(3, shinydashboard::valueBoxOutput(ns("kpi_total"), width = 12))
  )
}

mod_kpis_server <- function(id, data_filtrada) {
  moduleServer(id, function(input, output, session) {
    kpis <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      
      tibble::tibble(
        aprob = 100 * mean(df$estado_academico == "Aprobado", na.rm = TRUE),
        reprob = 100 * mean(df$estado_academico == "Reprobado", na.rm = TRUE),
        retiro = 100 * mean(df$estado_academico == "Retirado", na.rm = TRUE),
        rend = mean(df$nota_final, na.rm = TRUE),
        total = nrow(df)
      )
    })
    
    output$kpi_aprob <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = sprintf("%.1f%%", kpis()$aprob),
        subtitle = "Aprobación",
        icon = shiny::icon("check-circle"),
        color = "green"
      )
    })
    
    output$kpi_reprob <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = sprintf("%.1f%%", kpis()$reprob),
        subtitle = "Reprobación",
        icon = shiny::icon("times-circle"),
        color = "red"
      )
    })
    
    output$kpi_retiro <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = sprintf("%.1f%%", kpis()$retiro),
        subtitle = "Retiro",
        icon = shiny::icon("sign-out-alt"),
        color = "yellow"
      )
    })
    
    output$kpi_rend <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = sprintf("%.2f", kpis()$rend),
        subtitle = "Rendimiento promedio",
        icon = shiny::icon("chart-line"),
        color = "aqua"
      )
    })
    
    output$kpi_total <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = scales::comma(kpis()$total),
        subtitle = "Total evaluaciones",
        icon = shiny::icon("users"),
        color = "blue"
      )
    })
  })
}
