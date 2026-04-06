# Módulo de filtros

mod_filters_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(2, selectInput(ns("anio"), "Año", choices = NULL, multiple = TRUE)),
      column(2, selectInput(ns("ciclo"), "Ciclo", choices = NULL, multiple = TRUE)),
      column(2, selectInput(ns("cohorte"), "Cohorte", choices = NULL, multiple = TRUE)),
      column(2, selectInput(ns("curso"), "Curso", choices = NULL, multiple = TRUE)),
      column(2, selectInput(ns("area"), "Área", choices = NULL, multiple = TRUE)),
      column(2, selectInput(ns("modalidad"), "Modalidad", choices = NULL, multiple = TRUE))
    )
  )
}

mod_filters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      df <- data()
      req(df)

      updateSelectInput(session, "anio", choices = sort(unique(na.omit(df$anio_academico))))
      updateSelectInput(session, "ciclo", choices = sort(unique(na.omit(df$ciclo))))
      updateSelectInput(session, "cohorte", choices = sort(unique(na.omit(df$cohorte))))
      updateSelectInput(session, "curso", choices = sort(unique(na.omit(df$curso))))
      updateSelectInput(session, "area", choices = sort(unique(na.omit(df$area))))
      if ("modalidad" %in% names(df)) {
        updateSelectInput(session, "modalidad", choices = sort(unique(na.omit(df$modalidad))))
      }
    }, ignoreInit = FALSE)

    reactive({
      list(
        anio = input$anio,
        ciclo = input$ciclo,
        cohorte = input$cohorte,
        curso = input$curso,
        area = input$area,
        modalidad = input$modalidad
      )
    })
  })
}
