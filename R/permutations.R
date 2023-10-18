#' Permutations
#'
#' Compute permutations of a set of objects.
#'
#' @param n Numeric. Number of elements in the vector.
#' @param r Numeric. Number of elements to permute.
#' @param v vector or list containing the objects that should be permuted. Default sequence from 1 to n.
#' @param set Logical. If TRUE, remove duplicate values from the input vector.
#' @param repeats.allowed Logical. If TRUE, permutations with repetition are allowed.
#' @param count.permutations Logical. If TRUE, returns the number of identical permutations for each permutation.
#' @param out_format Character. The desired format for the output, either "matrix", "dataframe", "tibble", "datatable", or "auto".
#'
#' @return A matrix, dataframe, tibble, or datatable with the computed permutations.
#' @export
#'
#' @examples
#' permutations(n = 4, r = 2)
#' permutations(n = 4, r = 2, repeats.allowed = TRUE)
#' permutations(n = 4, r = 2, repeats.allowed = TRUE, count.permutations = TRUE)
permutations <- function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE, count.permutations = FALSE, out_format = 'auto') {
  if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 0)
    stop("bad value of n")
  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 0)
    stop("bad value of r")
  if (length(v) < n)
    stop("v is either non-atomic or too short")
  if ((r > n) & repeats.allowed == FALSE)
    stop("r > n and repeats.allowed=FALSE")
  if (set) {
    v <- unique(v)
    if(is.atomic(v))
      v <- sort(v)
    if (length(v) < n)
      stop("too few different elements")
  }

  if(isFALSE(out_format %in% c('auto', 'matrix', 'dataframe', 'tibble', 'datatable'))){
    warning('`out_format` should be one of ("auto", "matrix", "dataframe", "tibble", "datatable"). Setting to "auto".')
    out_format <- 'auto'
  }

  # Call to C++ functions for permutations
  if(repeats.allowed & count.permutations)
    out <- permutationsWithRepetition_counts(n, r)
  else if(repeats.allowed)
    out <- permutationsWithRepetition(n, r)
  else
    out <- permutationsWithoutRepetition(n, r)  # This function should be added to C++

  actual_format <- 'matrix'
  n_rows <- ifelse(repeats.allowed, n^r, factorial(n) / factorial(n - r))
  if (out_format == 'auto') {
    if (n_rows <= 1e2) {
      out_format <- "matrix"
    } else if (n_rows <= 1e5) {
      out_format <- "tibble"
    } else {
      out_format <- "datatable"
    }
  }

  if(count.permutations){
    col_names <- c(paste0("V", 1:r), 'n_permutations')
  } else{
    col_names <- paste0("V", 1:r)
  }
  colnames(out) <- col_names
  if (out_format == 'dataframe') {
    out <- as.data.frame(out)
    actual_format <- 'dataframe'

  } else if (out_format == 'tibble') {
    if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out, .name_repair = "minimal")
      actual_format <- 'tibble'
    } else {
      out <- as.data.frame(out)
      actual_format <- 'dataframe'
    }

  } else if (out_format == 'datatable') {
    if (requireNamespace("data.table", quietly = TRUE)) {
      out <- data.table::as.data.table(out)
      actual_format <- 'datatable'
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
      actual_format <- 'tibble'
    } else {
      out <- as.data.frame(out)
      actual_format <- 'dataframe'
    }
  }

  # If v was specified, let's plug the values in
  if(isFALSE(is.integer(v)) || any(v[1:n] !=(1:n))){
    if(actual_format == 'matrix'){
      if((isFALSE(count.permutations) & is.atomic(v)) || is.numeric(v)){
        out[,1:r] <- apply(out[,1:r], 2, map_vals_from_v, v=v[1:n])

      } else{
        warning('v is not numeric. Returning the matrix of combinations reporting the indices of v.
                To substitute the values of v automatically request a different output format.')
      }
    } else if(actual_format == 'datatable'){
      cols_to_mutate <- names(out)[grep("^V", names(out))]
      # out[, (cols_to_mutate) := lapply(.SD, map_vals_from_v,v=v[1:n]), .SDcols = cols_to_mutate]
      # Loop through the selected columns and apply the function using set
      for (col in cols_to_mutate) {
        data.table::set(out, j = col, value = map_vals_from_v(out[[col]], v = v[1:n]))
      }
    } else if(actual_format == 'tibble'){
      out <- dplyr::mutate(.data = out, dplyr::across(dplyr::starts_with('V'), .fns = \(x) map_vals_from_v(x, v = v[1:n])))
    } else {
      out[1:r] <- lapply(out[1:r], map_vals_from_v, v=v[1:n])
    }
  }

  return(out)
}
