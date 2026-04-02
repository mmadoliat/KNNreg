library(shiny)
library(ggplot2)

server.KNNreg <- function(input, output, session) {

  # Functions for splitting and plotting
  make_split <- function(data, prop = 0.7) {
    n <- nrow(data)
    train_idx <- sample(n, size = floor(prop * n))
    list(train = data[train_idx, ], test = data[-train_idx, ])
  }

  segment_df <- function(train_x, test_x, ids, k){
    train_x <- as.matrix(train_x)
    test_x  <- as.matrix(test_x)
    n <- nrow(train_x); p <- ncol(train_x); m <- nrow(test_x)
    test_x_out = test_x[rep(1:m, each = k), ]
    train_x_out = train_x[ids, ]
    out = as.data.frame(cbind(test_x_out, train_x_out))
    colnames(out) <- c(paste0("test_", colnames(train_x)), 
                       paste0("train_", colnames(train_x)))
    out
  }

  # Reactive train/test split
  split <- eventReactive(input$resample, {
    make_split(mtcars, prop = 0.92)
  }, ignoreNULL = FALSE)
  
  # Compute fitted (train) and predicted (test)
  fitted_vals <- reactive({
    d_train <- split()$train
    knn_pred(d_train[, c("wt", "qsec")], d_train[, c("mpg")], 
             d_train[, c("wt", "qsec")], input$k, input$backend)
  })
  predicted <- reactive({
    d_train <- split()$train
    d_test <- split()$test
    knn_pred(d_train[, c("wt", "qsec")], d_train[, c("mpg")], 
             d_test[, c("wt", "qsec")], input$k, input$backend)
  })
  
  mse <- function(obs, pred) mean((obs - pred)^2)
  r2  <- function(obs, pred) 1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))

  # Training performance
  output$trainPerf <- renderPrint({
    obs <- split()$train$mpg
    cat("Training set:\n")
    cat("  MSE =", round(mse(obs, fitted_vals()$preds), 3),
        "  R² =", round(r2(obs, fitted_vals()$preds), 3), "\n")
  })

  # Test performance
  output$testPerf <- renderPrint({
    obs <- split()$test$mpg
    cat("Test set:\n")
    cat("  MSE =", round(mse(obs, predicted()$preds), 3),
        "  R² =", round(r2(obs, predicted()$preds), 3), "\n")
  })

  # Plot showing k nearest neighbors
  output$predPlot <- renderPlot({
    d_train <- split()$train
    d_test <- split()$test
    d_test$id = 1:nrow(d_test)
    
    nn <- segment_df(d_train[, c("wt", "qsec")], 
                     d_test[, c("wt", "qsec")], 
                     predicted()$ids, input$k)
    
    d_test$pred <- predicted()$preds
    
    ggplot() +
      geom_segment(aes(x = test_wt, xend = train_wt, 
                       y = test_qsec, yend = train_qsec), 
                   color = "grey50", linetype = "dotted",
                   data = nn) +
      geom_point(aes(x = wt, y = qsec, color = mpg), 
                 size = 2, data = d_train) +
      geom_text(aes(x = wt, y = qsec, color = pred, label = id), 
                size = 5, fontface = "bold", data = d_test) +
      scale_color_gradient(low = "#6A00A8FF", high = "#FCA636FF") +
      theme_classic() +
      labs(x = "wt", y = "qsec")
  })

  # Table of test predictions
  output$predTable <- renderTable({
    d_test <- split()$test
    data.frame(
      id = 1:nrow(d_test),
      car = rownames(d_test),
      actual = round(d_test$mpg,2),
      predicted = round(predicted()$preds,2)
    )
  }, rownames = FALSE)
}
