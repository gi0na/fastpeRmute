# Test for basic functionality without repetitions
test_that("permutations without repetition works", {
  expect_equal(nrow(permutations(4, 2)), 12) # 4P2 = 12
  expect_equal(nrow(permutations(5, 3)), 60) # 5P3 = 60
})

# Test for basic functionality with repetitions
test_that("permutations with repetition works", {
  expect_equal(nrow(permutations(4, 2, repeats.allowed = TRUE)), 16) # 4^2 = 16
  expect_equal(nrow(permutations(5, 3, repeats.allowed = TRUE)), 125) # 5^3 = 125
})

# Test for unique values in output
test_that("permutations outputs unique rows", {
  res <- permutations(4, 2)
  expect_equal(nrow(res), length(unique(apply(res, 1, paste, collapse = ""))))
})

# Test for permutations with v parameter
test_that("permutations with custom v works", {
  res <- permutations(3, 2, v = c('A', 'B', 'C'))
  expect_true('A' %in% res[,1])
  expect_true('B' %in% res[,2])
})

# Testing with numerical v
test_that("permutations uses numerical v correctly", {
  n <- 3
  r <- 4
  v <- sample(1:50)

  result_without_v <- permutations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'matrix')
  result_without_v[,1:r] <- apply(result_without_v[,1:r], 2, map_vals_from_v, v = sort(v[1:n]))
  result_with_v <- permutations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'matrix', v = v[1:n])

  expect_equal(result_without_v, result_with_v)
})

# Testing with character v
test_that("permutations uses character v correctly", {
  n <- 3
  r <- 4
  v <- c(letters,LETTERS)[1:50]

  result_with_v_1 <- dplyr::arrange(
    permutations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'tibble', v = v[1:n]),
    V1,V2,V3,V4)
  result_with_v_2 <- dplyr::arrange(
    permutations(n = n, r = r, repeats.allowed = TRUE, count.permutations = FALSE, out_format = 'tibble', v = v[1:n]),
    V1,V2,V3,V4)

  expect_equal(result_with_v_1[,1:r], result_with_v_2)
})

# Testing with list v
test_that("permutations uses list v correctly", {
  n <- 3
  r <- 4
  v <- c(letters,LETTERS)[1:50]
  v <- mapply(v1 = v, v2 = sample(v), FUN = function(v1, v2) c(v1, v2), SIMPLIFY = FALSE)

  result_with_v_count <- dplyr::arrange(
    permutations(n = n, r = r, repeats.allowed = TRUE, count.permutations = TRUE, out_format = 'tibble', v = v[1:n]),
    V1,V2,V3,V4)
  result_with_v_no_count <- dplyr::arrange(
    permutations(n = n, r = r, repeats.allowed = TRUE, count.permutations = FALSE, out_format = 'tibble', v = v[1:n]),
    V1,V2,V3,V4)

  expect_equal(result_with_v_count[,1:r], result_with_v_no_count)
})


test_that("n_permutations is correct for all output formats in permutations", {
  order <- 13
  expected_val <- factorial(order)

  # Matrix
  permutations_mat <- fastpeRmute::permutations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'matrix')
  expect_equal(permutations_mat[, ncol(permutations_mat)][1], expected_val)

  # Data frame
  permutations_df <- fastpeRmute::permutations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'dataframe')
  expect_equal(permutations_df$n_permutations[1], expected_val)

  # Tibble
  permutations_tibble <- fastpeRmute::permutations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'tibble')
  expect_equal(permutations_tibble$n_permutations[[1]], expected_val)

  # Data table
  permutations_dt <- fastpeRmute::permutations(n = 3, r = order, repeats.allowed = T, count.permutations = T, out_format = 'datatable')
  expect_equal(permutations_dt$n_permutations[1], expected_val)
})

test_that("Permutation counts are correct", {
  n <- 3
  r <- 6

  # Compute permutations with counts
  perms_with_counts <- fastpeRmute::permutations(n = n, r = r, repeats.allowed = T, count.permutations = T, out_format = 'dataframe')

  # Compute permutations without counts
  perms_without_counts <- fastpeRmute::permutations(n = n, r = r, repeats.allowed = T, count.permutations = F, out_format = 'dataframe')

  # Drop the count column if present
  if ("n_permutations" %in% names(perms_without_counts)) {
    perms_without_counts$n_permutations <- NULL
  }

  # Calculate counts for each row
  computed_counts <- apply(perms_without_counts, 1, function(row) {
    freq <- table(row)
    count <- 1
    for (f in freq) {
      count <- count * factorial(f)
    }
    return(count)
  })

  # Compare these computed counts with counts from permutations with counts
  for (i in seq_along(perms_with_counts)) {
    expect_equal(perms_with_counts$n_permutations[i], computed_counts[i])
  }
})

test_that("permutations from fastpeRmute match gtools for various n, r values and repetition settings", {

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

    # With repetitions
    # Your function's output
    my_output_reps <- fastpeRmute:::permutationsWithRepetition(n, r)

    # gtools function's output
    gtools_output_reps <- gtools::permutations(n, r, repeats.allowed = TRUE)

    # Check that the outputs are the same
    expect_equal(my_output_reps, gtools_output_reps,
                 info = paste0("Failed for n=", n, " and r=", r, " with repetitions."))

    # Without repetitions
    # Your function's output
    my_output_no_reps <- fastpeRmute::permutations(n, r, out_format = 'tibble')
    my_output_no_reps <- as.matrix(dplyr::arrange(my_output_no_reps, dplyr::across(dplyr::starts_with('V'))))

    # gtools function's output
    gtools_output_no_reps <- gtools::permutations(n, r, repeats.allowed = FALSE)
    colnames(gtools_output_no_reps) <- colnames(my_output_no_reps)

    # Check that the outputs are the same
    expect_equal(my_output_no_reps, gtools_output_no_reps,
                 info = paste0("Failed for n=", n, " and r=", r, " without repetitions."))
  }
})
