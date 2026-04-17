# R/modules/institucional/ui.R
# Módulo: Análisis Institucional
mod_institucional_ui <- function() {
  shinydashboard::tabItem(
    tabName = "institucional",
    shiny::h2("🏫 Análisis Institucional - Visión Global"),
    
    # ---------------------------
    # Filtros Institucionales
    # ---------------------------
    shinydashboard::box(
      title = "🎛️ Filtros Institucionales (se recalcula al presionar 'Aplicar filtros')",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::sliderInput(
            "inst_rango_anos",
            "Rango de años",
            min = 2001, max = 2024, value = c(2001, 2024), step = 1, sep = ""
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "inst_cohortes",
            "Cohortes (año de ingreso)",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todas')
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "inst_modalidades",
            "Modalidades",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todas')
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "inst_periodos",
            "Tipo de período/ciclo",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todos')
          )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectizeInput(
            "inst_cursos",
            "Cursos",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todos', maxItems = 200)
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "inst_tipo_pensum",
            "Tipo de pensum",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todos')
          )
        ),
        shiny::column(
          3,
          shiny::selectizeInput(
            "inst_carreras",
            "Carrera",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todas')
          )
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectizeInput(
            "inst_departamentos",
            "Departamento",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todos')
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "inst_municipios",
            "Municipio",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'Todos')
          )
        ),
        shiny::column(
          2,
          shiny::checkboxInput("inst_solo_aprobados", "Solo aprobados (≥ 61)", value = FALSE)
        ),
        shiny::column(
          2,
          shiny::actionButton(
            "inst_aplicar_filtros",
            "Aplicar filtros",
            icon = shiny::icon("filter"),
            class = "btn-primary"
          )
        )
      ),
      
      shiny::hr(),
      
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::numericInput(
            "param_abandono_anos",
            "Parámetro: años para abandono",
            value = 2, min = 1, max = 10, step = 1
          )
        ),
        shiny::column(
          3,
          shiny::numericInput(
            "param_tiempo_estandar_anos",
            "Parámetro: tiempo estándar (años)",
            value = 6, min = 1, max = 15, step = 1
          )
        ),
        shiny::column(
          6,
          shiny::uiOutput("inst_resumen_filtros"),
          shiny::uiOutput("inst_diag_filtros")
        )
      )
    ),
    
    # ---------------------------
    # KPIs Institucionales
    # ---------------------------
    shiny::fluidRow(
      shinydashboard::valueBoxOutput("kpi_inst_estudiantes", width = 2),
      shinydashboard::valueBoxOutput("kpi_inst_cursos", width = 2),
      shinydashboard::valueBoxOutput("kpi_inst_aprobacion", width = 2),
      shinydashboard::valueBoxOutput("kpi_inst_reprobacion", width = 2),
      shinydashboard::valueBoxOutput("kpi_inst_graduados", width = 2),
      shinydashboard::valueBoxOutput("kpi_inst_abandono", width = 2)
    ),
    
    # ---------------------------
    # Visualizaciones
    # ---------------------------
    shinydashboard::tabBox(
      title = "📊 Visualizaciones Institucionales",
      width = 12,
      
      shiny::tabPanel(
        "Aprobación/Reprobación",
        plotly::plotlyOutput("inst_g_aprob_reprob_anual") %>% shinycssloaders::withSpinner(color = "#3498db"),
        shiny::hr(),
        plotly::plotlyOutput("inst_g_aprob_reprob_ciclo") %>% shinycssloaders::withSpinner(color = "#3498db")
      ),
      
      shiny::tabPanel(
        "Deserción (t+1 por ciclo)",
        plotly::plotlyOutput("inst_g_desercion_ciclo") %>% shinycssloaders::withSpinner(color = "#3498db"),
        shiny::hr(),
        DT::DTOutput("inst_tabla_desercion") %>% shinycssloaders::withSpinner(color = "#3498db")
      ),
      
      shiny::tabPanel(
        "Heatmap aprobación",
        plotly::plotlyOutput("inst_heatmap_aprob") %>% shinycssloaders::withSpinner(color = "#3498db")
      ),
      
      shiny::tabPanel(
        "Cohortes y eficiencia terminal",
        plotly::plotlyOutput("inst_g_cohortes") %>% shinycssloaders::withSpinner(color = "#3498db"),
        shiny::hr(),
        DT::DTOutput("inst_tabla_cohortes") %>% shinycssloaders::withSpinner(color = "#3498db")
      ),
      
      shiny::tabPanel(
        "Detalle",
        DT::DTOutput("inst_tabla_detalle") %>% shinycssloaders::withSpinner(color = "#3498db")
      )
    )
  )
}