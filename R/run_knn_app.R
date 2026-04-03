#' Launch the kNN Regression Shiny App
#'
#' Opens an interactive k-NN regression explorer.
#'
#' @importFrom shiny runApp
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme labs
#' @export
run_knn_app <- function() {
  app_path <- system.file("shiny/knn_app", package = "KNNreg")
  if (app_path == "") stop("Could not find the Shiny app. Reinstall the package.")
  shiny::runApp(app_path)
}
