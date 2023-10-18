#include <Rcpp.h>
using namespace Rcpp;

// Helper function to compute factorial
int factorial(int n) {
  return (n == 1 || n == 0) ? 1 : factorial(n - 1) * n;
}

// [[Rcpp::export]]
IntegerMatrix combinationsWithoutRepetition(int n, int r) {
  
  std::vector<int> v(n);
  
  for(int i = 0; i < n; ++i) {
    v[i] = i + 1;
  }
  
  if (r > n) {
    stop("r cannot be greater than the length of the vector.");
  }
  
  int numCombs = Rf_choose(n, r);
  IntegerMatrix result(numCombs, r);
  std::vector<int> indices(r);
  
  // Initialize the first combination
  for (int i = 0; i < r; i++) {
    indices[i] = i;
  }
  
  int row = 0;
  while (true) {
    // Write the combination to the result matrix
    for (int col = 0; col < r; col++) {
      result(row, col) = v[indices[col]];
    }
    row++;
    
    // Generate next combination
    int idx = r - 1;
    while (idx >= 0 && indices[idx] == n - r + idx) {
      idx--;
    }
    
    if (idx < 0) {
      break;
    }
    
    indices[idx]++;
    for (int j = idx + 1; j < r; j++) {
      indices[j] = indices[idx] + (j - idx);
    }
  }
  
  return result;
}

// [[Rcpp::export]]
IntegerMatrix permutationsWithoutRepetition(int n, int r) {
  
  // Step 1: Generate combinations
  IntegerMatrix combinations = combinationsWithoutRepetition(n, r);
  
  int numPermsEach = factorial(r);
  int numRows = combinations.nrow() * numPermsEach;
  
  // Resultant matrix
  IntegerMatrix result(numRows, r);
  
  // Step 2 and 3: For each combination, generate permutations and append to result
  for (int i = 0; i < combinations.nrow(); i++) {
    IntegerVector temp = combinations(i, _);
    std::sort(temp.begin(), temp.end());
    
    for (int j = 0; j < numPermsEach; j++) {
      for (int col = 0; col < r; col++) {
        result(i * numPermsEach + j, col) = temp[col];
      }
      std::next_permutation(temp.begin(), temp.end());
    }
  }
  
  return result;
}