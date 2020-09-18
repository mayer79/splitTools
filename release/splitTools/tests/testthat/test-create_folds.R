context("create_folds")

test_that("number of folds is correct", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 3)
  expect_equal(length(out), 3L)
})

test_that("no observation is lost", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 4)
  expect_equal(sort(unique(unlist(out))), seq_along(y))
})

test_that("no observation is doubled", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 4)
  expect_equal(length(unlist(out)), length(y) * 3)
})

test_that("folds have same size for integer n/k and basic splitting", {
  y <- rep(LETTERS[1:10], each = 10)
  k <- 4
  out <- create_folds(y, k = k, type = "basic")
  expect_equal(vapply(out, length, FUN.VALUE = 1, USE.NAMES = FALSE),
               rep(length(y) - length(y) / k, k))
})

test_that("stratified splitting is balanced for integer n/k within stratum", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 2, type = "stratified")
  expect_equal(length(unique(table(y[out[[1]]]))), 1)
})

test_that("argument 'invert' works", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 4, invert = TRUE)
  expect_equal(length(unlist(out)), length(y))
})

test_that("argument 'seed' works", {
  y <- rep(LETTERS[1:10], each = 10)
  expect_equal(create_folds(y, k = 3, seed = 1),
               create_folds(y, k = 3, seed = 1))
})

test_that("argument 'use_names' works", {
  y <- rep(LETTERS[1:10], each = 10)
  expect_named(create_folds(y, k = 3, use_names = TRUE), c("Fold1", "Fold2", "Fold3"))
  expect_named(create_folds(y, k = 3, use_names = FALSE), NULL)
})

test_that("argument 'm_rep' works", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 5, m_rep = 2)
  expect_equal(length(out), 2 * 5)
})

test_that("blocked splitting works", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_folds(y, k = 4, type = "blocked", invert = TRUE)
  expect_equal(out[[1]], 1:25)
})

test_that("grouped splitting works", {
  y <- rep(LETTERS[1:2], each = 50)
  out <- create_folds(y, k = 2, type = "grouped", invert = TRUE)
  expect_true(all(out[[1]] == 1:50) || all(out[[2]] == 1:50))
})

test_that("basic splitting works with seed", {
  y <- rep(LETTERS[1:10], each = 5)
  out <- create_folds(y, k = 3, type = "basic", seed = 1)
  expect_equal(out[[1]][1:7], c(2, 4, 5, 6, 8, 9, 10))
})

test_that("stratified splitting works with seed", {
  y <- rep(LETTERS[1:10], each = 5)
  out <- create_folds(y, k = 3, type = "stratified", seed = 1, invert = TRUE)
  expect_equal(out[[1]][1:7], c(1, 5, 8, 10, 12, 15, 17))
})

test_that("grouped splitting works with seed", {
  y <- rep(LETTERS[1:10], each = 5)
  out <- create_folds(y, k = 3, type = "grouped", seed = 1)
  expect_equal(out[[1]][1:10], c(1:5, 11:15))
})

test_that("character and factor input give same result", {
  y <- rep(LETTERS[1:10], each = 5)
  k <- 3
  out1 <- create_folds(y, k = k, type = "grouped", seed = 1)
  out2 <- create_folds(factor(y), k = k, type = "grouped", seed = 1)
  expect_equal(out1, out2)
})

test_that("stratified splitting on continous data works with missing values", {
  y <- c(1:99, NA)
  out <- create_folds(y, k = 2, seed = 1)
  expect_equal(length(unlist(out)), length(y))
})

test_that("stratified splitting on continous data reacts on n_bins", {
  y <- rep(1:10, each = 10)
  out1 <- create_folds(y, k = 2, n_bins = 2, seed = 1)
  out2 <- create_folds(y, k = 2, n_bins = 50, seed = 1)
  expect_equal(isTRUE(all.equal(out1, out2)), FALSE)
  expect_equal(diff(vapply(out2, function(z) mean(y[z]),
                           FUN.VALUE = numeric(1), USE.NAMES = FALSE)), 0)
})


