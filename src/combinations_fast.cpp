#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix combinationsWithRepetition(int n, int r) {
  
  std::vector<int> v(n);
  
  for(int i = 0; i < n; ++i) {
    v[i] = i + 1;
  }
  
  int numCombs = Rf_choose(n + r - 1, r);
  IntegerMatrix result(numCombs, r);
  std::vector<int> indices(r, 0);
  int row = 0;
  
  while (true) {
    // Write the combination to the result matrix
    for (int col = 0; col < r; col++) {
      result(row, col) = v[indices[col]];
    }
    row++;
    
    // Update the indices for the next combination
    int idx = r - 1;
    while (idx >= 0 && indices[idx] == n - 1) {
      idx--;
    }
    
    if (idx < 0) {
      break;
    }
    
    indices[idx]++;
    for (int j = idx + 1; j < r; j++) {
      indices[j] = indices[idx];
    }
  }
  
  return result;
}