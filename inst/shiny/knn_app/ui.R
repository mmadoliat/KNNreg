library(shiny)

ui.KNNreg <- fluidPage(
  titlePanel("k-NN Regression Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Number of neighbors (k):",
                  min = 1, max = 20, value = 5, step = 1),
      radioButtons("backend", "Compute backend:",
                   choices = c("R","cpp"), selected = "cpp"),
      actionButton("resample", "New train/test split"),
      hr(),
      h4("Performance"),
      verbatimTextOutput("trainPerf"),
      verbatimTextOutput("testPerf")
    ),
    mainPanel(
      plotOutput("predPlot", height = "400px"),
      tableOutput("predTable")
    )
  )
)
