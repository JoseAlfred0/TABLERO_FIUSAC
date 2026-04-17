# R/data/cache.R
# Caché global de datos para evitar recargar el dataset por sesión
.datos_cache_env <- new.env(parent = emptyenv())

obtener_mtime_seguro <- function(path) {
  tryCatch(file.info(path)$mtime, error = function(e) NA)
}

obtener_desde_cache_mtime <- function(clave_datos, clave_mtime, mtime_actual) {
  if (exists(clave_datos, envir = .datos_cache_env, inherits = FALSE) &&
      exists(clave_mtime, envir = .datos_cache_env, inherits = FALSE) &&
      isTRUE(identical(get(clave_mtime, envir = .datos_cache_env, inherits = FALSE), mtime_actual))) {
    return(get(clave_datos, envir = .datos_cache_env, inherits = FALSE))
  }
  NULL
}

guardar_en_cache_mtime <- function(clave_datos, clave_mtime, datos, mtime_actual) {
  assign(clave_datos, datos, envir = .datos_cache_env)
  assign(clave_mtime, mtime_actual, envir = .datos_cache_env)
}