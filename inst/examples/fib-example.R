rec_fib_r <- function(n) {
  if (n < 2) return(n)
  return(rec_fib_r(n - 1) + rec_fib_r(n - 2))
}

# Iterative Fibonacci in R (more efficient than recursive)
fib_r <- function(n) {
  if (n <= 1) return(n)
  a <- 0
  b <- 1
  for (i in 2:n) {
    temp <- b
    b <- a + b
    a <- temp
  }
  return(b)
}

library(Rcpp)
sourceCpp("./inst/examples/fib-helper.cpp")

library(rbenchmark)
benchmark(rec_fib_r(15), rec_fib_r(20), rec_fib_r(25))[, 1:4]
benchmark(rec_fib_r(25), rec_fib_cpp(25))[, 1:4]

benchmark(fib_cpp(25), fib_r(25), rec_fib_cpp(25), rec_fib_r(25))[, 1:4]


library(microbenchmark)
microbenchmark(R = fib_r(47), Rcpp = fib_cpp(47), times = 100L)
microbenchmark(R = fib_r(92), Rcpp = fib_cpp(92), times = 100L)
