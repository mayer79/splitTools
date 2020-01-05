// https://gallery.rcpp.org/articles/stl-random-shuffle/index.html
#include <Rcpp.h>
using namespace Rcpp;

// wrapper around R's RNG such that we get a uniform distribution over
// [0,n) as required by the STL algorithm
inline int randWrapper(const int n) { return floor(unif_rand() * n); }

// [[Rcpp::export]]
Rcpp::NumericVector C_fast_shuffle(Rcpp::NumericVector x, double n) {
  // clone x into out to leave a alone
  Rcpp::NumericVector out = clone(x);

  std::random_shuffle(out.begin(), out.end(), randWrapper);

  if (out.size() > n) {
    out = out[Rcpp::Range(0, n - 1)];
  }

  return out;
}
