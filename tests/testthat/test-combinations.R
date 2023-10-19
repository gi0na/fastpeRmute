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

  result_with_v_1 <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'tibble', v = v[1:n])
  result_with_v_2 <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = FALSE, out_format = 'tibble', v = v[1:n])

  expect_equal(result_with_v_1[,1:r], result_with_v_2)
})

# Testing with list v
test_that("combinations uses list v correctly", {
  n <- 3
  r <- 4
  v <- c(letters,LETTERS)[1:50]
  v <- mapply(v1 = v, v2 = sample(v), FUN = function(v1, v2) c(v1, v2), SIMPLIFY = FALSE)

  result_with_v_count <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'tibble', v = v[1:n])
  result_with_v_no_count <- combinations(n = n, r = r, repeats.allowed = TRUE, count.permutations = FALSE, out_format = 'tibble', v = v[1:n])

  expect_equal(result_with_v_count[,1:r], result_with_v_no_count)
})


test_that("n_permutations is correct for all output formats", {
  order <- 13
  expected_val <- factorial(order)

  # Matrix
  combinations_mat <- fastpeRmute::combinations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'matrix')
  expect_equal(combinations_mat[, ncol(combinations_mat)][1], expected_val)

  # Data frame
  combinations_df <- fastpeRmute::combinations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'dataframe')
  expect_equal(combinations_df$n_permutations[1], expected_val)

  # Tibble
  combinations_tibble <- fastpeRmute::combinations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'tibble')
  expect_equal(combinations_tibble$n_permutations[[1]], expected_val)

  # Data table
  combinations_dt <- fastpeRmute::combinations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'datatable')
  expect_equal(combinations_dt$n_permutations[1], expected_val)
})


  # Calculate counts for each row
test_that("Permutation counts are correct", {
  n <- 3
  r <- 6

  # Compute combinations with counts
  combs_with_counts <- fastpeRmute::combinations(n = n, r = r, repeats.allowed = T, count.permutations = T, out_format = 'dataframe')

  # Compute combinations without counts
  combs_without_counts <- fastpeRmute::combinations(n = n, r = r, repeats.allowed = T, count.permutations = F, out_format = 'dataframe')

  # Drop the count column if present
  if ("n_permutations" %in% names(combs_without_counts)) {
    combs_without_counts$n_permutations <- NULL
  }

  # Calculate counts for each row
  computed_counts <- apply(combs_without_counts, 1, function(row) {
    freq <- table(row)
    count <- 1
    for (f in freq) {
      count <- count * factorial(f)
    }
    return(count)
  })

  # Compare these computed counts with counts from permutations with counts
  for (i in seq_along(combs_with_counts)) {
    expect_equal(combs_with_counts$n_permutations[i], computed_counts[i])
  }
})

test_that("combinations with repetitions from fastpeRmute match gtools for various n and r values", {

  # Test parameters
  test_values <- list(
    c(10, 4),
    c(5, 2),
    c(7, 3),
    c(8, 5)
  )

  for (values in test_values) {
    n <- values[1]
    r <- values[2]

    # Your function's output
    my_output <- fastpeRmute:::combinationsWithRepetition(n, r)

    # gtools function's output
    gtools_output <- gtools::combinations(n, r, repeats.allowed = TRUE)

    # Check that the outputs are the same
    expect_equal(my_output, gtools_output,
                 info = paste0("Failed for n=", n, " and r=", r))
  }
})

test_that("combinations from fastpeRmute match combn for various n and r values", {

  # Test parameters
  test_values <- list(
    c(10, 4),
    c(5, 2),
    c(7, 3),
    c(8, 5)
  )

  for (values in test_values) {
    n <- values[1]
    r <- values[2]

    my_output <- fastpeRmute:::combinationsWithoutRepetition(n, r)

    # Base R combn function's output
    base_output <- t(combn(x = n, m = r))

    # Check that the outputs are the same
    expect_equal(my_output, base_output,
                 info = paste0("Failed for n=", n, " and r=", r))
  }
})

