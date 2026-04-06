# TABLERO_FIUSAC

Tablero interactivo en **R + Shiny** para indicadores de eficacia académica de la Escuela de Ciencias y Sistemas (FIUSAC), con enfoque reproducible para datos históricos 2001-2024.

## Alcance analítico
- Aprobación
- Reprobación
- Retiro
- Deserción (proxy por no reinscripción)
- Rendimiento promedio
- Indicadores complementarios según disponibilidad (cohorte, área, modalidad)

## Estructura del proyecto

```text
├── data/
│   ├── raw/
│   └── processed/
├── R/
│   ├── 01_explorar_datos.R
│   ├── 02_limpieza_y_etl.R
│   ├── 03_analisis_descriptivo.R
│   ├── helpers_data_import.R
│   ├── helpers_processing.R
│   ├── helpers_metrics.R
│   └── helpers_plots.R
├── app/
│   ├── app.R
│   ├── global.R
│   ├── ui.R
│   ├── server.R
│   └── modules/
├── docs/
│   ├── diccionario_datos.md
│   ├── manual_tecnico.md
│   └── resumen_exploracion.md
├── outputs/
│   ├── tables/
│   └── plots/
├── renv/
├── .gitignore
├── run_app.R
└── README.md
```

## Fuentes de datos
Ubicar archivos base en `data/raw/`:
- `estudiantes.csv` o `estudiantes.xlsx`/`estudiantes.xls`
- `notas.csv` o `notas.xlsx`/`notas.xls`

## Instalación
1. Instalar R y RStudio.
2. Desde la raíz del proyecto, inicializar dependencias:

```r
install.packages("renv")
renv::init(bare = TRUE)
renv::install(c(
  "shiny", "tidyverse", "ggplot2", "plotly", "DT", "lubridate",
  "janitor", "readr", "readxl", "arrow", "glue", "broom", "bslib", "scales"
))
renv::snapshot()
```

## Ejecución del flujo
```r
source("R/01_explorar_datos.R")
source("R/02_limpieza_y_etl.R")
source("R/03_analisis_descriptivo.R")
source("run_app.R")
```

## Restauración en otra máquina
```r
install.packages("renv")
renv::restore()
```

## Actualización de datos
1. Reemplazar insumos en `data/raw/`.
2. Ejecutar scripts 01 → 02 → 03.
3. Abrir app con `source("run_app.R")`.

## Flujo de trabajo recomendado
- Exploración y validación inicial.
- ETL y documentación del diccionario.
- Análisis descriptivo/inferencial base.
- Iteración de visualizaciones en Shiny.
- Versionado con Git por fase.
