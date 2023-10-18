# Test for basic functionality without repetitions
test_that("combinations without repetition works", {
  expect_equal(nrow(combinations(4, 2)), 6) # 4C2 = 6
  expect_equal(nrow(combinations(5, 3)), 10) # 5C3 = 10
})

# Test for basic functionality with repetitions
test_that("combinations with repetition works", {
  expect_equal(nrow(combinations(4, 2, repeats.allowed = TRUE)), choose(4+2-1,2))
  expect_equal(nrow(combinations(5, 3, repeats.allowed = TRUE)), choose(5+3-1,3))
})

# Test for unique values in output without repetitions
test_that("combinations outputs unique rows (no repeats)", {
  res <- combinations(4, 2)
  expect_equal(nrow(res), length(unique(apply(res, 1, paste, collapse = ""))))
})

# Test for unique values in output with repetitions
test_that("combinations outputs unique rows (with repeats)", {
  res <- combinations(4, 2, repeats.allowed = TRUE)
  expect_equal(nrow(res), length(unique(apply(res, 1, paste, collapse = ""))))
})

# Test for combinations with v parameter
test_that("combinations with custom v works", {
  res <- combinations(3, 2, v = c('A', 'B', 'C'))
  expect_true('A' %in% res[,1])
  expect_true('B' %in% res[,2])
})

# Test for count.permutations
test_that("combinations with count.permutations works", {
  res <- combinations(3, 2, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'dataframe')
  expect_equal(sum(res$n_permutations), nrow(combinations(3, 2, repeats.allowed = TRUE)) * 2)
})

# Test error conditions (like r > n without repeats)
test_that("combinations throws errors when expected", {
  expect_error(combinations(2, 3, repeats.allowed = FALSE))
})


# Testing with numerical v
test_that("combinations uses numerical v correctly", {
  n <- 3
  r <- 4
  v <- sample(1:50)

  result_without_v <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'matrix')
  result_without_v[,1:r] <- apply(result_without_v[,1:r], 2, map_vals_from_v, v = sort(v[1:n]))
  result_with_v <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'matrix', v = v[1:n])

  expect_equal(result_without_v, result_with_v)
})

# Testing with character v
test_that("combinations uses character v correctly", {
  n <- 3
  r <- 4
  v <- c(letters,LETTERS)[1:50]

  result_with_v_1 <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'matrix', v = v[1:n])
  result_with_v_2 <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = FALSE, out_format = 'matrix', v = v[1:n])

  expect_equal(result_with_v_1[,1:r], result_with_v_2)
})

# Testing with list v
test_that("combinations uses list v correctly", {
  n <- 3
  r <- 4
  v <- mapply(v1 = v, v2 = sample(v), FUN = function(v1, v2) c(v1, v2), SIMPLIFY = FALSE)

  result_with_v_count <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'matrix', v = v[1:n])
  result_with_v_no_count <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = FALSE, out_format = 'matrix', v = v[1:n])

  expect_equal(result_with_v_count[,1:r], result_with_v_no_count)
})

# Testing different output formats
formats <- c('dataframe', 'tibble', 'datatable')

for (fmt in formats) {
  test_that(paste0("combinations uses v correctly with ", fmt, " format"), {
    result_without_v <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = fmt)
    result_with_v <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = fmt, v = v[1:n])

    # For datatable, comparing directly sometimes fails due to internal structure differences.
    # Convert to dataframe for the comparison.
    if (fmt == 'datatable') {
      result_without_v <- as.data.frame(result_without_v)
      result_with_v <- as.data.frame(result_with_v)
    }

    expect_equal(result_without_v, result_with_v)
  })
}

