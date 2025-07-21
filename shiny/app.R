library(shiny)

ui <- fluidPage(
  fileInput("file1", "Choose CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")),
  tableOutput("contents")
)

server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
}

shinyApp(ui, server)
