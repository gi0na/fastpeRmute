#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix permutationsWithRepetition(int n, int r) {
  
  std::vector<int> v(n);
  
  for(int i = 0; i < n; ++i) {
    v[i] = i + 1;
  }
  // The total number of permutations with repetition is n^r
  int totalPermutations = std::pow(n, r);
  IntegerMatrix out(totalPermutations, r);
  
  for (int i = 0; i < totalPermutations; ++i) {
    int temp = i;
    for (int j = r - 1; j >= 0; --j) {
      out(i, j) = v[temp % n];
      temp /= n;
    }
  }
  
  return out;
}