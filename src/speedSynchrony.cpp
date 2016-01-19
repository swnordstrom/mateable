#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix pair_sync_aug_noself(NumericVector syncMatrix, NumericVector durMatrix,
                            int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j) continue;

      int ind = i+1;
      int jnd = j+1;
      if (ind > jnd) {
        ind = j+1;
        jnd = i+1;
      }
      int jmp = j;
      if (j > i) jmp = j-1;
      pairSync(i, jmp) = syncMatrix[n*(ind-1)-ind*(ind+1)/2+jnd-1]/durMatrix[i];
    }
  }

  return pairSync;
}

// [[Rcpp::export]]
NumericMatrix pair_sync_aug_self(NumericVector syncMatrix, NumericVector durMatrix,
                                   int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      int ind = i+1;
      int jnd = j+1;
      if (ind > jnd) {
        ind = j+1;
        jnd = i+1;
      }
      pairSync(i, j) = syncMatrix[n*(ind-1)-ind*(ind-1)/2+jnd-1]/durMatrix[i];
    }
  }

  return pairSync;
}

// [[Rcpp::export]]
NumericMatrix pair_sync_either_noself(NumericVector syncMatrix, int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j) continue;

      int ind = i+1;
      int jnd = j+1;
      if (ind > jnd) {
        ind = j+1;
        jnd = i+1;
      }
      int jmp = j;
      if (j > i) jmp = j-1;
      pairSync(i, jmp) = syncMatrix[n*(ind-1)-ind*(ind+1)/2+jnd-1];
    }
  }

  return pairSync;
}

// [[Rcpp::export]]
NumericMatrix pair_sync_either_self(NumericVector syncMatrix, int n) {
  NumericMatrix pairSync(n, n-1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      int ind = i+1;
      int jnd = j+1;
      if (ind > jnd) {
        ind = j+1;
        jnd = i+1;
      }
      pairSync(i, j) = syncMatrix[n*(ind-1)-ind*(ind-1)/2+jnd-1];
    }
  }

  return pairSync;
}
