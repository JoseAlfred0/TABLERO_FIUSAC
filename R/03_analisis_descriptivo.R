#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})

source(file.path("R", "helpers_metrics.R"))
source(file.path("R", "helpers_plots.R"))

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)

master_path <- "data/processed/datos_fiusac_procesados.rds"
if (!file.exists(master_path)) {
  stop("No existe data/processed/datos_fiusac_procesados.rds. Ejecute primero R/02_limpieza_y_etl.R")
}

master <- readRDS(master_path)

safe_group_summary <- function(df, group_col) {
  if (!(group_col %in% names(df))) return(tibble())

  df |>
    group_by(.data[[group_col]]) |>
    summarise(
      total = n(),
      promedio_nota = mean(nota_final, na.rm = TRUE),
      aprobacion = 100 * mean(estado_academico == "Aprobado", na.rm = TRUE),
      reprobacion = 100 * mean(estado_academico == "Reprobado", na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(total))
}

summaries <- list(
  cohorte = safe_group_summary(master, "cohorte"),
  anio = safe_group_summary(master, "anio_academico"),
  ciclo = safe_group_summary(master, "ciclo"),
  curso = safe_group_summary(master, "curso"),
  area = safe_group_summary(master, "area")
)

purrr::iwalk(summaries, ~ readr::write_csv(.x, file.path("outputs/tables", paste0("resumen_", .y, ".csv"))))

metrics <- calculate_base_metrics(master)

p_aprob <- plot_trend(metrics, "aprobacion", "Tendencia de aprobación 2001-2024", "Aprobación (%)")
p_reprob <- plot_trend(metrics, "reprobacion", "Tendencia de reprobación 2001-2024", "Reprobación (%)")
p_rend <- plot_trend(metrics, "rendimiento_promedio", "Rendimiento promedio por año", "Nota promedio")

course_col <- if ("curso" %in% names(master)) "curso" else names(master)[1]
p_top <- plot_top_reprobacion(master, course_col)

for (obj in list(
  aprobacion = p_aprob,
  reprobacion = p_reprob,
  rendimiento = p_rend,
  top_reprobacion = p_top
)) {
  name <- names(obj)
  ggplot2::ggsave(
    filename = file.path("outputs/plots", paste0(name, ".png")),
    plot = obj[[1]],
    width = 10,
    height = 5,
    dpi = 150
  )
}

inferencias <- c()

nota_no_na <- master$nota_final[!is.na(master$nota_final)]
if (length(nota_no_na) >= 3 && length(nota_no_na) <= 5000) {
  shapiro_res <- shapiro.test(nota_no_na)
  inferencias <- c(
    inferencias,
    paste0("Shapiro-Wilk p-value: ", signif(shapiro_res$p.value, 4), " (n=", length(nota_no_na), ").")
  )
} else {
  inferencias <- c(
    inferencias,
    "Shapiro-Wilk no ejecutado: tamaño de muestra fuera de rango recomendado (3 a 5000)."
  )
}

if ("area" %in% names(master) && dplyr::n_distinct(master$area, na.rm = TRUE) > 1) {
  anova_df <- master |>
    filter(!is.na(area), !is.na(nota_final))

  if (nrow(anova_df) > 5) {
    fit <- aov(nota_final ~ area, data = anova_df)
    anova_tbl <- broom::tidy(fit)
    readr::write_csv(anova_tbl, "outputs/tables/anova_area.csv")
    inferencias <- c(inferencias, "ANOVA por área ejecutado. Revisar outputs/tables/anova_area.csv.")
  } else {
    inferencias <- c(inferencias, "ANOVA por área no ejecutado: muestra insuficiente con nota y área.")
  }
} else {
  inferencias <- c(inferencias, "ANOVA por área no ejecutado: variable área no disponible o sin variabilidad.")
}

limitations <- c(
  "- Resultados inferenciales son exploratorios y sensibles a calidad de datos históricos.",
  "- Si hay no normalidad o heterocedasticidad, interpretar ANOVA con precaución.",
  "- La deserción se aproxima por no reinscripción interanual cuando aplica."
)

writeLines(
  c(
    "# Hallazgos descriptivos e inferenciales iniciales",
    "",
    "## Pruebas estadísticas",
    paste0("- ", inferencias),
    "",
    "## Limitaciones",
    limitations
  ),
  "outputs/tables/limitaciones_inferencia.md"
)

cat("Análisis descriptivo finalizado. Ver outputs/tables y outputs/plots.\n")
