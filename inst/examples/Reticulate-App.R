library(shiny)
library(reticulate)
ui <- fluidPage(
  actionButton("btn", "Compute in Python"),
  verbatimTextOutput("out")
)
server <- function(input, output) {
  observeEvent(input$btn, {
    # Run Python code when button is clicked
    py_run_string("import math; res = math.factorial(10)")
    output$out <- renderText(py$res)
  })
}
shinyApp(ui, server)
