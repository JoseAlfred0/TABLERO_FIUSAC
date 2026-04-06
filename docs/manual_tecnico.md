# Manual técnico - Tablero FIUSAC

## 1. Objetivo
Implementar un tablero interactivo para monitorear eficacia académica (aprobación, reprobación, retiro, rendimiento y deserción proxy) para Ingeniería en Ciencias y Sistemas FIUSAC con datos 2001-2024.

## 2. Flujo técnico
1. Exploración inicial: `R/01_explorar_datos.R`
2. ETL y tabla maestra: `R/02_limpieza_y_etl.R`
3. Análisis descriptivo e inferencial inicial: `R/03_analisis_descriptivo.R`
4. Visualización interactiva: `run_app.R`

## 3. Requisitos
- R >= 4.3 recomendado.
- Paquetes administrados por `renv`.
- Archivos de entrada en `data/raw/` con nombres base `estudiantes` y `notas` (csv/xlsx/xls).

## 4. Estructura de datos procesados
El artefacto principal es:
- `data/processed/datos_fiusac_procesados.rds`

Columnas derivadas clave:
- `nota_final`
- `anio_academico`
- `ciclo`
- `cohorte`
- `estado_academico`

## 5. Reglas de negocio
- Aprobado: `nota_final >= 61`
- Reprobado: `nota_final < 61`
- Retirado: detección por patrón textual en estado original (`reti`, `aband`, `desert`)

## 6. Módulos Shiny
- `mod_filters`: filtros globales.
- `mod_kpis`: tarjetas KPI filtrables.
- `mod_timeseries`: serie temporal aprob/reprob.
- `mod_boxplots`: distribución por curso y área.
- `mod_heatmap`: desempeño curso vs ciclo.
- `mod_top_courses`: top 10 cursos críticos.

## 7. Consideraciones de rendimiento
- Carga de datos procesados una sola vez en `global.R`.
- Cálculos reactivos sobre subconjuntos filtrados.
- ETL offline para evitar recálculo pesado en la app.

## 8. Despliegue futuro
- Contenerizar con `rocker/shiny`.
- Publicar en Shiny Server / Posit Connect.
- Programar refresco ETL (cron) y versionado de insumos.
