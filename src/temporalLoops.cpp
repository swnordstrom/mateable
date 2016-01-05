#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sync_loop_nodiag(IntegerVector starts, IntegerVector ends, int n) {
  IntegerVector syncMatrix(n*(n-1)/2);
  int count = 0;
  for (int i = 0; i < n-1; i++) {
    for (int j = i+1; j < n; j++) {
      int begin;
      int finish;
      if (ends[i] < ends[j]) {
        finish = ends[i];
      } else {
        finish = ends[j];
      }
      if (starts[i] < starts[j]) {
        begin = starts[j];
      } else {
        begin = starts[i];
      }

      int days = finish - begin;
      if (days > 0) days += 1;
      if (days < 0) days = 0;
//       int ind = i+1;
//       int jnd = j+1;
//       n*(ind-1) - ind*(ind+1)/2 + jnd - 1 should equal count
      syncMatrix[count] = days;
      count++;
    }
  }
  return syncMatrix;
}

// [[Rcpp::export]]
IntegerVector sync_loop_diag(IntegerVector starts, IntegerVector ends, int n) {
  IntegerVector syncMatrix(n*(n+1)/2);
  int count = 0;
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      int begin;
      int finish;
      if (ends[i] < ends[j]) {
        finish = ends[i];
      } else {
        finish = ends[j];
      }
      if (starts[i] < starts[j]) {
        begin = starts[j];
      } else {
        begin = starts[i];
      }

      int days = finish - begin;
      if (days > 0) days += 1;
      if (days < 0) days = 0;
//       int ind = i+1;
//       int jnd = j+1;
//       n*(ind-1) - ind*(ind-1)/2 + jnd - 1 should equal count
      syncMatrix[count] = days;
      count++;
    }
  }
  return syncMatrix;
}

// [[Rcpp::export]]
IntegerVector either_loop_nodiag(IntegerVector starts, IntegerVector ends, int n) {
  IntegerVector eitherMatrix(n*(n-1)/2);
  int count = 0;
  for (int i = 0; i < n-1; i++) {
    for (int j = i+1; j < n; j++) {
      int begin;
      int finish;
      if (ends[i] > ends[j]) {
        finish = ends[i];
      } else {
        finish = ends[j];
      }
      if (starts[i] > starts[j]) {
        begin = starts[j];
      } else {
        begin = starts[i];
      }

      int days = finish - begin;
      if (days > 0) days += 1;
      if (days < 0) days = 0;
//       int ind = i+1;
//       int jnd = j+1;
//       n*(ind-1) - ind*(ind+1)/2 + jnd - 1 should equal count
      eitherMatrix[count] = days;
      count++;
    }
  }
  return eitherMatrix;
}

// [[Rcpp::export]]
IntegerVector either_loop_diag(IntegerVector starts, IntegerVector ends, int n) {
  IntegerVector eitherMatrix(n*(n+1)/2);
  int count = 0;
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      int begin;
      int finish;
      if (ends[i] > ends[j]) {
        finish = ends[i];
      } else {
        finish = ends[j];
      }
      if (starts[i] > starts[j]) {
        begin = starts[j];
      } else {
        begin = starts[i];
      }

      int days = finish - begin;
      if (days > 0) days += 1;
      if (days < 0) days = 0;
//       int ind = i+1;
//       int jnd = j+1;
//       n*(ind-1) - ind*(ind-1)/2 + jnd - 1 should equal count
      eitherMatrix[count] = days;
      count++;
    }
  }
  return eitherMatrix;
}
