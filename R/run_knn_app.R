#' Launch the kNN Regression Shiny App
#'
#' Opens an interactive k-NN regression explorer.
#'
#' @export
run_knn_app <- function() {
  app_path <- system.file("shiny/knn_app", package = "KNNreg")
  if (app_path == "") stop("Could not find the Shiny app. Reinstall the package.")
  shiny::runApp(app_path)
}
