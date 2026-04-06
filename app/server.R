app_server <- function(input, output, session) {
  data_master <- reactiveVal(load_master_data())

  filtros <- mod_filters_server("flt", reactive(data_master()))

  data_filtrada <- reactive({
    apply_filters(data_master(), filtros())
  })

  mod_kpis_server("kpi", data_filtrada)
  mod_kpis_server("kpi_resumen", data_filtrada)
  mod_timeseries_server("ts", data_filtrada)
  mod_boxplots_server("box", data_filtrada)
  mod_heatmap_server("hm", data_filtrada)
  mod_top_courses_server("top", data_filtrada)

  output$tabla_detalle <- DT::renderDT({
    df <- data_filtrada()
    validate(need(nrow(df) > 0, "Sin datos para mostrar."))
    DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
}
