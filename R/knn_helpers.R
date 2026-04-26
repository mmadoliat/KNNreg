#' Split data into training and test sets
#'
#' @param data A data frame.
#' @param prop Proportion of data for training set (default 0.7).
#' @return A list with `train` and `test` data frames.
#' @keywords internal

make_split <- function(data, prop = 0.7) {
  n <- nrow(data)
  train_idx <- sample(n, size = floor(prop * n))
  list(train = data[train_idx, ], test = data[-train_idx, ])
}


#' Create a data frame of test points and their k nearest neighbors
#'
#' @param train_x Numeric matrix/data frame of training predictors.
#' @param test_x Numeric matrix/data frame of test predictors.
#' @param ids Indices of nearest neighbors for each test point.
#' @param k Number of neighbors.
#' @return Data frame suitable for plotting segments.
#' @keywords internal

segment_df <- function(train_x, test_x, ids, k){
  train_x <- as.matrix(train_x)
  test_x  <- as.matrix(test_x)
  
  n <- nrow(train_x); p <- ncol(train_x); m <- nrow(test_x)
  
  # Create df with each test point alongside k nearest neighbors
  test_x_out = test_x[rep(1:m, each = k), ]
  train_x_out = train_x[ids, ]
  out = as.data.frame(cbind(test_x_out, train_x_out))
  colnames(out) <- c(paste0("test_", colnames(train_x)), 
                     paste0("train_", colnames(train_x)))
  out
}
