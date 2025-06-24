# write a shiny app that takes a csv file as input and displays a table of the data

library(shiny)

ui <- fluidPage(
  titlePanel("CSV File Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
}

shinyApp(ui, server)
