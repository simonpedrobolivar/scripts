// [[Rcpp::depends(RcppArmadillo)]]
//#include <Rcpp.h>
#include <RcppArmadillo.h>
//#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;



// [[Rcpp::export()]]

arma::mat calc_L(arma::mat A, arma::mat I) {
  return( inv(I - A)) ;
}


