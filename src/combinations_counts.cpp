#include <Rcpp.h>
using namespace Rcpp;

// This remains unchanged, but you'll want to use a double vector to hold the factorial values
std::vector<double> factorialTable;

void initializeFactorialTable(int r) {
  factorialTable.resize(r + 1);
  factorialTable[0] = 1;
  for (double i = 1; i <= r; i++) {
    factorialTable[i] = i * factorialTable[i - 1];
  }
}

// [[Rcpp::export]]
List combinationsWithRepetition_counts(int n, int r) {

  std::vector<int> v(n);
  for(int i = 0; i < n; ++i) {
    v[i] = i + 1;
  }

  int numCombs = Rf_choose(n + r - 1, r);
  IntegerMatrix result(numCombs, r);
  NumericVector combProductValues(numCombs);

  initializeFactorialTable(r);

  std::vector<int> indices(r, 0);
  std::vector<int> freq(n, 0);

  for (int row = 0; row < numCombs; row++) {
    std::fill(freq.begin(), freq.end(), 0);
    double combProduct = 1;

    for (int col = 0; col < r; col++) {
      result(row, col) = v[indices[col]];
      freq[indices[col]]++;
    }

    for (int i = 0; i < n; i++) {
      combProduct *= factorialTable[freq[i]];
    }

    combProductValues[row] = combProduct;

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

  // Returning a List containing the matrix and the vector of double values
  return List::create(_["combinations"] = result, _["factorialValues"] = combProductValues);
}


// [[Rcpp::export]]
List permutationsWithRepetition_counts(int n, int r) {

  std::vector<int> v(n);
  for(int i = 0; i < n; ++i) {
    v[i] = i + 1;
  }

  int numPerms = std::pow(n, r); // n^r permutations with repetition
  IntegerMatrix result(numPerms, r);
  NumericVector identicalPermsValues(numPerms);

  initializeFactorialTable(r);

  for (int row = 0; row < numPerms; row++) {
    int temp = row;
    std::vector<int> freq(n, 0);
    double identicalPerms = 1; // start with 1 as if all elements were distinct

    for (int col = 0; col < r; col++) {
      int index = temp % n;
      result(row, col) = v[index];
      freq[index]++;
      temp /= n;
    }

    for (int i = 0; i < n; i++) {
      identicalPerms *= factorialTable[freq[i]];
    }

    identicalPermsValues[row] = identicalPerms;
  }

  // Returning a List containing the matrix and the vector of double values
  return List::create(_["permutations"] = result, _["factorialValues"] = identicalPermsValues);
}
