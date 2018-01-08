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
Rcpp::List create_timeseries(Rcpp::List list_of_matrices) {
  for(int i = 0; i < )
  for(i in 1:(n_ind*n_countries)){
    for(j in 1:(n_ind*n_countries)){
      timeseries[[length(timeseries)+1]] <- sapply(A_list, 
                         FUN = function(x) return(x[i,j])) # get timeseries of coefficient in row i and col j 
      names(timeseries)[[length(timeseries)]] <- paste(industriesXcountries[i], "->", industriesXcountries[j])
      print(paste("i =", i, "j =", j)) # very slow
    }
  }
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
