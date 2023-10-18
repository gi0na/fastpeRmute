#' Combinations
#'
#' Compute combinations from a vector.
#'
#' @param n Numeric. Number of elements in the vector.
#' @param r Numeric. Number of elements to choose.
#' @param v Numeric vector. Default sequence from 1 to n.
#' @param set Logical. If TRUE, remove duplicate values from the input vector.
#' @param repeats.allowed Logical. If TRUE, combinations with repetition are allowed.
#' @param count.permutations Logical. If TRUE, returns the number of permutations corresponding to each combination.
#' @param out_format Character. Either 'auto' or another desired format. 'auto' will try to convert to a data.table.
#'
#' @return A data.table or matrix with the computed combinations.
#' @export
#'
#' @examples
#' combinations(n = 4, r = 2)
#' combinations(n = 4, r = 2, repeats.allowed = TRUE)
#' combinations(n = 4, r = 2, count.permutations = TRUE)
combinations <- function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE, count.permutations = FALSE, out_format = 'auto') {
  if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 0)
    stop("bad value of n")
  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 0)
    stop("bad value of r")
  if (!is.atomic(v) || length(v) < n)
    stop("v is either non-atomic or too short")
  if ((r > n) & repeats.allowed == FALSE)
    stop("r > n and repeats.allowed=FALSE")
  if (set) {
    v <- unique(sort(v))
    if (length(v) < n)
      stop("too few different elements")
  }

  if( repeats.allowed & count.permutations )
    out <- combinationsWithRepetition_counts(n,r)
  else if(repeats.allowed)
    out <- combinationsWithRepetition(n,r)
  else if(count.permutations)
    out <- combinationsWithoutRepetition_counts(n,r)
  else
    out <- combinationsWithoutRepetition(n,r)

  if(out_format == 'auto'){
    require(data.table)
    out <- data.table::as.data.table(out)
    if(count.permutations){
      data.table::setnames(out, c(paste0("V", 1:r),'n_permutations'))
    } else{
      data.table::setnames(out, paste0("V", 1:r))
    }
  }

  return(out)
}
