#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int rec_fib_cpp(int n) {
  if (n < 2) return(n);
  return(rec_fib_cpp(n - 1) + rec_fib_cpp(n - 2)); 
}

// [[Rcpp::export]]
long long fib_cpp(int n) {
  if (n <= 1) return n;
  long long a = 0, b = 1, temp;
  for (int i = 2; i <= n; ++i) {
    temp = b;
    b = a + b;
    a = temp;
  }
  return b;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
all.equal(fib_cpp(15), fib_r(15), rec_fib_cpp(15), rec_fib_r(15))
*/
