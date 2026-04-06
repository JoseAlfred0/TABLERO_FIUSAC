# Módulo series de tiempo

mod_timeseries_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("ts_plot"), height = "420px")
}

mod_timeseries_server <- function(id, data_filtrada) {
  moduleServer(id, function(input, output, session) {
    output$ts_plot <- renderPlotly({
      df <- data_filtrada()
      validate(need(nrow(df) > 0, "Sin datos para los filtros seleccionados."))

      ts <- df |>
        dplyr::group_by(anio_academico) |>
        dplyr::summarise(
          aprobacion = 100 * mean(estado_academico == "Aprobado", na.rm = TRUE),
          reprobacion = 100 * mean(estado_academico == "Reprobado", na.rm = TRUE),
          .groups = "drop"
        )

      p <- ggplot2::ggplot(ts, ggplot2::aes(x = anio_academico)) +
        ggplot2::geom_line(ggplot2::aes(y = aprobacion, color = "Aprobación"), linewidth = 1.1) +
        ggplot2::geom_line(ggplot2::aes(y = reprobacion, color = "Reprobación"), linewidth = 1.1) +
        ggplot2::labs(title = "Serie de tiempo de indicadores", x = "Año", y = "%", color = "Indicador") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p, tooltip = c("x", "y", "colour"))
    })
  })
}
