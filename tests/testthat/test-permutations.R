context("Testing permutations function")

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

# Test for count.permutations
test_that("permutations with count.permutations works", {
  res <- permutations(3, 2, repeats.allowed = TRUE, count.permutations = TRUE)
  expect_equal(sum(res$n_permutations), nrow(permutations(3, 2, repeats.allowed = TRUE)) * 2)
})

