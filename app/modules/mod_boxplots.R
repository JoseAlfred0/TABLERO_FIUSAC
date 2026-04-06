# Módulo boxplots

mod_boxplots_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, plotlyOutput(ns("box_curso"), height = "420px")),
    column(6, plotlyOutput(ns("box_area"), height = "420px"))
  )
}

mod_boxplots_server <- function(id, data_filtrada) {
  moduleServer(id, function(input, output, session) {
    output$box_curso <- renderPlotly({
      df <- data_filtrada()
      validate(need("curso" %in% names(df), "No existe columna curso."))
      validate(need(nrow(df) > 0, "Sin datos para mostrar."))

      top <- df |>
        dplyr::count(curso, sort = TRUE) |>
        dplyr::slice_head(n = 15) |>
        dplyr::pull(curso)

      p <- df |>
        dplyr::filter(curso %in% top, !is.na(nota_final)) |>
        ggplot2::ggplot(ggplot2::aes(x = reorder(curso, nota_final, FUN = median), y = nota_final)) +
        ggplot2::geom_boxplot(fill = "#6baed6") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Distribución de nota por curso (top 15)", x = "Curso", y = "Nota") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    output$box_area <- renderPlotly({
      df <- data_filtrada()
      validate(need("area" %in% names(df), "No existe columna área."))
      validate(need(nrow(df) > 0, "Sin datos para mostrar."))

      p <- df |>
        dplyr::filter(!is.na(area), !is.na(nota_final)) |>
        ggplot2::ggplot(ggplot2::aes(x = area, y = nota_final, fill = area)) +
        ggplot2::geom_boxplot(show.legend = FALSE) +
        ggplot2::labs(title = "Distribución de nota por área", x = "Área", y = "Nota") +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })
  })
}
