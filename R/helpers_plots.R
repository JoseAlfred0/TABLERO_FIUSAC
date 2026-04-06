# Helpers de visualización

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

plot_trend <- function(df, yvar, title, ylab) {
  ggplot(df, aes(x = anio_academico, y = .data[[yvar]])) +
    geom_line(color = "#1f77b4", linewidth = 1) +
    geom_point(color = "#1f77b4", size = 1.8) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    labs(title = title, x = "Año", y = ylab) +
    theme_minimal(base_size = 12)
}

plot_top_reprobacion <- function(df, course_col) {
  df |>
    filter(!is.na(.data[[course_col]]), !is.na(estado_academico)) |>
    group_by(.data[[course_col]]) |>
    summarise(
      total = n(),
      reprobados = sum(estado_academico == "Reprobado", na.rm = TRUE),
      tasa = 100 * reprobados / pmax(total, 1),
      .groups = "drop"
    ) |>
    arrange(desc(tasa), desc(total)) |>
    slice_head(n = 10) |>
    ggplot(aes(x = reorder(.data[[course_col]], tasa), y = tasa)) +
    geom_col(fill = "#d62728") +
    coord_flip() +
    labs(
      title = "Top 10 cursos con mayor reprobación",
      x = "Curso",
      y = "Tasa de reprobación (%)"
    ) +
    theme_minimal(base_size = 12)
}
