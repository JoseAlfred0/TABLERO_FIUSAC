app_ui <- function(request) {
  fluidPage(
    titlePanel("Tablero de Eficacia Académica - FIUSAC"),
    p("Ingeniería en Ciencias y Sistemas | Serie histórica 2001-2024"),
    
    mod_filters_ui("flt"),
    br(),
    mod_kpis_ui("kpi"),
    br(),
    
    tabsetPanel(
      tabPanel("Resumen general", mod_kpis_ui("kpi_resumen")),
      tabPanel("Series de tiempo", mod_timeseries_ui("ts")),
      tabPanel("Rendimiento por curso y área", mod_boxplots_ui("box")),
      tabPanel(
        "Riesgo académico / cursos críticos",
        fluidRow(column(7, mod_heatmap_ui("hm")), column(5, mod_top_courses_ui("top")))
      ),
      tabPanel("Tabla detallada", DT::DTOutput("tabla_detalle"))
    )
  )
}
