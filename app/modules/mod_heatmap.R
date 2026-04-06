# Módulo heatmap

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("heatmap"), height = "460px")
}

mod_heatmap_server <- function(id, data_filtrada) {
  moduleServer(id, function(input, output, session) {
    output$heatmap <- renderPlotly({
      df <- data_filtrada()
      validate(need(all(c("curso", "ciclo") %in% names(df)), "Heatmap requiere columnas curso y ciclo."))
      validate(need(nrow(df) > 0, "Sin datos para mostrar."))

      hm <- df |>
        dplyr::filter(!is.na(curso), !is.na(ciclo)) |>
        dplyr::group_by(curso, ciclo) |>
        dplyr::summarise(rendimiento = mean(nota_final, na.rm = TRUE), .groups = "drop")

      p <- ggplot2::ggplot(hm, ggplot2::aes(x = ciclo, y = curso, fill = rendimiento,
                                            text = paste0("Curso: ", curso, "<br>Ciclo: ", ciclo, "<br>Promedio: ", round(rendimiento, 2)))) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
        ggplot2::labs(title = "Heatmap de rendimiento por curso vs ciclo", x = "Ciclo", y = "Curso", fill = "Nota") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p, tooltip = "text")
    })
  })
}
