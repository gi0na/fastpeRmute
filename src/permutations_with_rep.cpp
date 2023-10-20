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

// Version with preallocation of memory, risky.
// #include <Rcpp.h>
// using namespace Rcpp;

// // Returns n raised to the power of r
// int int_pow(int n, int r) {
//     int result = 1;
//     for (int i = 0; i < r; ++i) {
//         result *= n;
//     }
//     return result;
// }

// // [[Rcpp::export]]
// IntegerMatrix permutationsWithRepetition(int n, int r) {
//   static int previous_r = -1; // To check if the matrix size changed from the previous call
//   static IntegerMatrix result; // This will prevent memory reallocation for same 'r'

//   int totalRows = int_pow(n, r); // use our integer power function
  
//   if (r != previous_r) {
//     result = IntegerMatrix(totalRows, r);
//     previous_r = r;
//   }
  
//   for (int row = 0; row < totalRows; row++) {
//     int tempRow = row;
//     for (int col = r - 1; col >= 0; --col) {
//       result(row, col) = tempRow % n + 1;
//       tempRow /= n;
//     }
//   }

//   return result;
// }
