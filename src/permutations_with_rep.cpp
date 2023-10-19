#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix permutationsWithRepetition(int n, int r) {
  int totalRows = pow(n, r);
  IntegerMatrix result(totalRows, r);

  for (int row = 0; row < totalRows; row++) {
    int tempRow = row;
    for (int col = r - 1; col >= 0; --col) {
      result(row, col) = tempRow % n + 1;
      tempRow /= n;
    }
  }

  return result;
}
