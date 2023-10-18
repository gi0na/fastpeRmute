#' @exportPattern "^[[:alpha:]]+"
#' @importFrom Rcpp evalCpp
#' @useDynLib fastpeRmute, .registration = TRUE
NULL

map_vals_from_v <- function(vals,v){
  v[vals]
}
