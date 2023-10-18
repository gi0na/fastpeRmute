#include <Rcpp.h>
using namespace Rcpp;

std::vector<int> factorialTable;

void initializeFactorialTable(int n) {
  factorialTable.resize(n + 1);
  factorialTable[0] = 1;
  for (int i = 1; i <= n; i++) {
    factorialTable[i] = i * factorialTable[i - 1];
  }
}

// [[Rcpp::export]]
IntegerMatrix combinationsWithRepetition_counts(int n, int r) {
  
  std::vector<int> v(n);
  
  for(int i = 0; i < n; ++i) {
    v[i] = i + 1;
  }
  int numCombs = Rf_choose(n + r - 1, r);
  IntegerMatrix result(numCombs, r + 1);
  
  initializeFactorialTable(r);
  
  std::vector<int> indices(r, 0);
  std::vector<int> freq(n, 0);
  
  for (int row = 0; row < numCombs; row++) {
    std::fill(freq.begin(), freq.end(), 0);
    int combProduct = 1;
    
    for (int col = 0; col < r; col++) {
      result(row, col) = v[indices[col]];
      freq[indices[col]]++;
    }
    
    for (int i = 0; i < n; i++) {
      combProduct *= factorialTable[freq[i]];
    }
    
    result(row, r) = combProduct;
    
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
