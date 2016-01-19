#include <Rcpp.h>
#include <std.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix pair_sync_aug_noself(IntegerVector syncMatrix, IntegerVector durMatrix,
                            int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j) continue;

      int ind = i;
      int jnd = j;
      if (ind > jnd) {
        ind = j;
        jnd = i;
      }
      cout << "i " << i;
      cout << "j " << j;
      pairSync(i, j) = syncMatrix[n*(ind-1)-ind*(ind+1)/2+jnd]/durMatrix[i];
    }
  }

  return pairSync;
}

// [[Rcpp::export]]
NumericMatrix pair_sync_aug_self(IntegerVector syncMatrix, IntegerVector durMatrix,
                                   int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      int ind = i;
      int jnd = j;
      if (ind > jnd) {
        ind = j;
        jnd = i;
      }
      pairSync(i, j) = syncMatrix[n*(ind-1)-ind*(ind-1)/2+jnd]/durMatrix[i];
    }
  }

  return pairSync;
}


// [[Rcpp::export]]
NumericMatrix pair_sync_eith_noself(IntegerVector syncMatrix, IntegerVector durMatrix,
                                   int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j) continue;

      int ind = i;
      int jnd = j;
      if (ind > jnd) {
        ind = j;
        jnd = i;
      }
      pairSync(i, j) = syncMatrix[n*(ind-1)-ind*(ind+1)/2+jnd];
    }
  }

  return pairSync;
}

// [[Rcpp::export]]
NumericMatrix pair_sync_eith_self(IntegerVector syncMatrix, IntegerVector durMatrix,
                                 int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      int ind = i;
      int jnd = j;
      if (ind > jnd) {
        ind = j;
        jnd = i;
      }
      pairSync(i, j) = syncMatrix[n*(ind-1)-ind*(ind-1)/2+jnd];
    }
  }

  return pairSync;
}
