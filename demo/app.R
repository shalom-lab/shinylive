library(shiny)

ui <- fluidPage(
  p('hello world')
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)