# Helpers de métricas académicas

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

calculate_base_metrics <- function(df) {
  df |>
    filter(!is.na(estado_academico)) |>
    group_by(anio_academico, ciclo) |>
    summarise(
      total_evaluados = n(),
      aprobados = sum(estado_academico == "Aprobado", na.rm = TRUE),
      reprobados = sum(estado_academico == "Reprobado", na.rm = TRUE),
      retirados = sum(estado_academico == "Retirado", na.rm = TRUE),
      rendimiento_promedio = mean(nota_final, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      aprobacion = 100 * aprobados / pmax(total_evaluados, 1),
      reprobacion = 100 * reprobados / pmax(total_evaluados, 1),
      retiro = 100 * retirados / pmax(total_evaluados, 1)
    )
}

calculate_dropout_proxy <- function(df, id_col = "id_estudiante") {
  if (!(id_col %in% names(df)) || !"anio_academico" %in% names(df)) {
    return(tibble(anio_academico = integer(), desercion_proxy = numeric()))
  }

  ids_by_year <- df |>
    filter(!is.na(.data[[id_col]]), !is.na(anio_academico)) |>
    distinct(anio_academico, .data[[id_col]])

  years <- sort(unique(ids_by_year$anio_academico))

  out <- lapply(seq_along(years)[-length(years)], function(i) {
    y <- years[i]
    next_y <- years[i + 1]

    current_ids <- ids_by_year |>
      filter(anio_academico == y) |>
      pull(.data[[id_col]]) |>
      unique()

    next_ids <- ids_by_year |>
      filter(anio_academico == next_y) |>
      pull(.data[[id_col]]) |>
      unique()

    no_reinscripcion <- sum(!current_ids %in% next_ids)
    desercion_proxy <- 100 * no_reinscripcion / pmax(length(current_ids), 1)

    tibble(anio_academico = y, desercion_proxy = desercion_proxy)
  })

  bind_rows(out)
}
