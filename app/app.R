source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

ui <- app_ui
server <- app_server

shinyApp(ui, server)
