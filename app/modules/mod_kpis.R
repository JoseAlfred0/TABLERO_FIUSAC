# Módulo KPIs

mod_kpis_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(2, valueBoxOutput(ns("kpi_aprob"))),
    column(2, valueBoxOutput(ns("kpi_reprob"))),
    column(2, valueBoxOutput(ns("kpi_retiro"))),
    column(3, valueBoxOutput(ns("kpi_rend"))),
    column(3, valueBoxOutput(ns("kpi_total")))
  )
}

mod_kpis_server <- function(id, data_filtrada) {
  moduleServer(id, function(input, output, session) {
    kpis <- reactive({
      df <- data_filtrada()
      req(nrow(df) > 0)
      total <- nrow(df)
      tibble::tibble(
        aprob = 100 * mean(df$estado_academico == "Aprobado", na.rm = TRUE),
        reprob = 100 * mean(df$estado_academico == "Reprobado", na.rm = TRUE),
        retiro = 100 * mean(df$estado_academico == "Retirado", na.rm = TRUE),
        rend = mean(df$nota_final, na.rm = TRUE),
        total = total
      )
    })

    output$kpi_aprob <- renderValueBox({
      bslib::value_box(title = "Aprobación", value = sprintf("%.1f%%", kpis()$aprob), theme = "success")
    })
    output$kpi_reprob <- renderValueBox({
      bslib::value_box(title = "Reprobación", value = sprintf("%.1f%%", kpis()$reprob), theme = "danger")
    })
    output$kpi_retiro <- renderValueBox({
      bslib::value_box(title = "Retiro", value = sprintf("%.1f%%", kpis()$retiro), theme = "warning")
    })
    output$kpi_rend <- renderValueBox({
      bslib::value_box(title = "Rendimiento promedio", value = sprintf("%.2f", kpis()$rend), theme = "primary")
    })
    output$kpi_total <- renderValueBox({
      bslib::value_box(title = "Total evaluaciones", value = scales::comma(kpis()$total), theme = "secondary")
    })
  })
}
