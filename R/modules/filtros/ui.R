# R/modules/filtros/ui.R
# Módulo: Filtros inteligentes
mod_filtros_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinydashboard::box(
      title = "🎯 Filtros Inteligentes",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      
      shiny::fluidRow(
        shiny::column(4,
                      shiny::selectizeInput(
                        ns("seccion_activa"),
                        "Sección de Análisis:",
                        choices = c(
                          "Análisis Individual" = "individual",
                          "Análisis Institucional" = "institucional",
                          "Series de Tiempo" = "series"
                        ),
                        selected = "individual"
                      )
        ),
        shiny::column(8,
                      shiny::uiOutput(ns("filtros_contextuales"))
        )
      ),
      
      shiny::hr(),
      
      # Filtros comunes a todas las secciones
      shiny::fluidRow(
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_rango_anos"),
                        "Rango de Años:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Seleccione años...',
                          plugins = list('remove_button')
                        )
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_modalidad"),
                        "Modalidad:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Todas las modalidades...',
                          plugins = list('remove_button')
                        )
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_periodo"),
                        "Período:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Todos los períodos...',
                          plugins = list('remove_button')
                        )
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("filtro_curso_grupo"),
                        "Curso (grupo):",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Todos los cursos...',
                          maxOptions = 100,
                          plugins = list('remove_button')
                        )
                      )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(6,
                      shiny::actionButton(ns("aplicar_filtros"), "Aplicar Filtros", 
                                          icon = shiny::icon("filter"), 
                                          class = "btn-primary btn-block",
                                          style = "margin-top: 10px;")
        ),
        shiny::column(6,
                      shiny::actionButton(ns("limpiar_filtros"), "Limpiar Todo", 
                                          icon = shiny::icon("broom"), 
                                          class = "btn-default btn-block",
                                          style = "margin-top: 10px;")
        )
      )
    )
  )
}