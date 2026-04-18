# Módulo: Series de Tiempo
mod_series_ui <- function() {
  shinydashboard::tabItem(
    tabName = "series",
    shiny::h2("📈 Series de Tiempo - Indicadores"),
    
    # Filtros (se recalcula al presionar 'Aplicar filtros')
    shinydashboard::box(
      title = "🧰 Filtros Series de Tiempo (se recalcula al presionar 'Aplicar filtros')",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::sliderInput(
            "ser_rango_anos",
            "Rango de años",
            min = 2001,
            max = 2024,
            value = c(2001, 2024),
            sep = ""
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "ser_granularidad",
            "Granularidad",
            choices = c("Año", "Ciclo/Periodo", "Cohorte"),
            selected = "Año",
            multiple = FALSE,
            options = list(placeholder = "Seleccione...")
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "ser_indicador",
            "Indicador",
            choices = c(
              "Promedio",
              "% Aprobación",
              "% Reprobación",
              "Deserción (t+1 por ciclo)",
              "Eficiencia terminal (cohorte)"
            ),
            selected = "Promedio",
            multiple = FALSE,
            options = list(placeholder = "Seleccione...")
          )
        ),
        shiny::column(
          3,
          shiny::actionButton(
            "ser_aplicar_filtros",
            "Aplicar filtros",
            icon = shiny::icon("filter"),
            class = "btn-primary",
            width = "100%"
          )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_modalidades",
            "Modalidades",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todas")
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_periodos",
            "Tipo de período/ciclo",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todos")
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_cursos",
            "Cursos",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todos")
          )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_tipo_pensum",
            "Tipo de pensum",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todos")
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_carreras",
            "Carrera",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todas")
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_cohortes",
            "Cohortes (año de ingreso)",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todas")
          )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_departamentos",
            "Departamento",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todos")
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "ser_municipios",
            "Municipio",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Todos")
          )
        ),
        shiny::column(
          4,
          shiny::checkboxInput(
            "ser_promedio_solo_aprobados",
            "Promedio solo aprobados (≥ 61)",
            value = TRUE
          ),
          shiny::checkboxInput(
            "ser_suavizado",
            "Suavizado (media móvil 3)",
            value = FALSE
          )
        )
      ),
      
      shiny::uiOutput("ser_resumen_filtros"),
      shiny::uiOutput("ser_diag_filtros")
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shinydashboard::box(
          title = "📈 Serie",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotly::plotlyOutput("grafico_series") %>%
            shinycssloaders::withSpinner(color = "#3498db")
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shinydashboard::box(
          title = "📋 Tabla",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          DT::DTOutput("tabla_series") %>%
            shinycssloaders::withSpinner(color = "#3498db")
        )
      )
    )
  )
}