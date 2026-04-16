# tests/smoke_test.R
# Prueba rápida: verifica que app.R parsea y que existen ui/server.
app_path <- file.path('..', 'app.R')
source(app_path, local = TRUE)
stopifnot(exists('ui'), exists('server'))
cat('OK: ui/server encontrados.\n')
