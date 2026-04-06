# Módulo top cursos críticos

mod_top_courses_ui <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("top_table"))
}

mod_top_courses_server <- function(id, data_filtrada) {
  moduleServer(id, function(input, output, session) {
    output$top_table <- DT::renderDT({
      df <- data_filtrada()
      validate(need("curso" %in% names(df), "No existe columna curso."))
      validate(need(nrow(df) > 0, "Sin datos para mostrar."))

      top <- df |>
        dplyr::filter(!is.na(curso)) |>
        dplyr::group_by(curso) |>
        dplyr::summarise(
          total = dplyr::n(),
          reprobados = sum(estado_academico == "Reprobado", na.rm = TRUE),
          tasa_reprobacion = 100 * reprobados / pmax(total, 1),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(tasa_reprobacion), dplyr::desc(total)) |>
        dplyr::slice_head(n = 10)

      DT::datatable(top, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}
