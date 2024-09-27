#' Compute Combinations of Elements
#'
#' This function calculates combinations of a set of objects. It offers flexibility in allowing for
#' combinations with repetition, as well as returning counts of corresponding permutations.
#' It also provides different output formats tailored for further processing or analysis.
#'
#' @param n Numeric. The number of elements in the input set. If `v` is provided, `n` is ignored.
#' @param r Numeric. The number of elements selected for each combination.
#' @param v A vector or list containing the objects to be combined. If not specified,
#'          it defaults to a sequence from 1 to `n`.
#' @param set Logical. If TRUE, the function will remove duplicate values from the input vector `v`.
#' @param repeats.allowed Logical. Determines whether combinations with repetition are allowed.
#'                         Default is FALSE.
#' @param count.permutations Logical. If set to TRUE, an additional column will be appended to the output
#'                           indicating the number of permutations corresponding to each combination.
#' @param out_format A character string specifying the desired format for the output. Valid options are
#'                   "matrix", "dataframe", "tibble", "datatable", or "auto". If "auto" is chosen, the function
#'                   will decide the most suitable format based on other input parameters.
#'
#' @return Depending on the `out_format` specified, the function returns combinations as a matrix,
#'         dataframe, tibble, or datatable. If `count.permutations` is TRUE, the last column of the output
#'         will indicate the number of permutations corresponding to each combination.
#'
#' @export
#'
#' @examples
#' # Basic combination without repetition
#' combinations(n = 4, r = 2)
#'
#' # Combination with repetition
#' combinations(n = 4, r = 2, repeats.allowed = TRUE)
#'
#' # Combination with repetition and count of corresponding permutations
#' combinations(n = 4, r = 2, repeats.allowed = TRUE, count.permutations = TRUE)
#'
#' # Using a custom vector of objects for combinations
#' combinations(v = c("apple", "banana", "cherry"), r = 2)
combinations <- function (n = NULL, r, v = NULL, set = TRUE, repeats.allowed = FALSE,
                          count.permutations = FALSE, out_format = 'auto') {

  # If neither n nor v is provided, throw an error
  if (is.null(n) && is.null(v)) {
    stop("Either 'n' or 'v' must be provided.")
  }

  # If v is provided and n is not, set n to the length of v
  if (is.null(n) && !is.null(v)) {
    n <- length(v)
  } else if (!is.null(n) && is.null(v)) {
    v <- 1:n
  }

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
  if(isFALSE(repeats.allowed) & count.permutations){
    warning('There can be no permutations without repeats.')
    count.permutations <- FALSE
  }

  if(repeats.allowed & count.permutations){
    out <- combinationsWithRepetition_counts(n,r)
    factorialValues <- out$factorialValues
    out <- out$combinations
  }
  else if(repeats.allowed)
    out <- combinationsWithRepetition(n,r)
  else
    out <- combinationsWithoutRepetition(n,r)

  actual_format <- 'matrix'
  n_rows <- ifelse(repeats.allowed, choose(n+r-1,r), choose(n,r))
  if (out_format == 'auto') {
    if (n_rows <= 1e2) {
      out_format <- "matrix"
    } else if (n_rows <= 1e5) {
      out_format <- "tibble"
    } else {
      out_format <- "datatable"
    }
  }


  col_names <- paste0("V", 1:r)
  colnames(out) <- col_names

  if(out_format == 'matrix' & count.permutations){
    out <- cbind(out, n_permutations = factorialValues)
  } else if (out_format == 'dataframe') {
    out <- as.data.frame(out)
    if(count.permutations){
      out$n_permutations <- factorialValues
    }
    actual_format <- 'dataframe'

  } else if (out_format == 'tibble') {
    if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out, .name_repair = "minimal")
      actual_format <- 'tibble'
    } else {
      out <- as.data.frame(out)
      actual_format <- 'dataframe'
    }
    if(count.permutations){
      out$n_permutations <- factorialValues
    }

  } else if (out_format == 'datatable') {
    if (requireNamespace("data.table", quietly = TRUE)) {
      out <- data.table::as.data.table(out)
      if(count.permutations){
        data.table::set(out, j = "n_permutations", value = factorialValues)
      }
      actual_format <- 'datatable'
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
      if(count.permutations){
        out$n_permutations <- factorialValues
      }
      actual_format <- 'tibble'
    } else {
      out <- as.data.frame(out)
      if(count.permutations){
        out$n_permutations <- factorialValues
      }
      actual_format <- 'dataframe'
    }
  }

  # If v was specified, let's plug the values in
  if(isFALSE(is.integer(v)) || any(v[1:n] !=(1:n))){
    if(actual_format == 'matrix'){
      if((isFALSE(count.permutations) & is.atomic(v)) || is.numeric(v)){
        if(nrow(out)==1){
          out[,1:r] <- map_vals_from_v(vals = out, v=v[1:n])
        } else{
          out[,1:r] <- apply(out[,1:r], 2, map_vals_from_v, v=v[1:n])
        }

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
