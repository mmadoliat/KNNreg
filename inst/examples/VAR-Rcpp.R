rSim <- function(B, E) {
  X <- matrix(0, nrow(E), ncol(E))
  for (r in 2:nrow(E)) {
    X[r, ] = X[r-1, ] %*% B + E[r, ]
  }
  return(X)
}

Rcpp::cppFunction('arma::mat cppSim(arma::mat B, arma::mat E) {
  int m = E.n_rows, n = E.n_cols;
  arma::mat X(m, n);
  X.row(0) = arma::zeros<arma::mat>(1, n);
  for (int r = 1; r < m; r++) {
    X.row(r) = X.row(r-1) * B + E.row(r);
  }
  return X;
}', depends="RcppArmadillo")

a <- matrix(c(0.5, 0.1, 0.1, 0.5), nrow = 2)
e <- matrix(rnorm(10000), ncol = 2)
rbenchmark::benchmark(cppSim(a, e), rSim(a, e), order="relative")[, 1:4]
