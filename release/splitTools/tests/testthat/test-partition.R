context("partition")

test_that("number of partitions is correct", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- partition(y, p = c(0.6, 0.2, 0.2))
  expect_equal(length(out), 3L)
})

test_that("no observation is lost", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- partition(y, p = c(0.6, 0.2, 0.2))
  expect_equal(sort(unique(unlist(out))), seq_along(y))
})

test_that("no observation is doubled", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- partition(y, p = c(0.6, 0.2, 0.2))
  expect_equal(length(unlist(out)), length(y))
})

test_that("partitions are exact for integer n*p and basic splitting", {
  y <- rep(LETTERS[1:10], each = 10)
  p <- c(0.6, 0.2, 0.2)
  out <- partition(y, p, type = "basic")
  expect_equal(vapply(out, length, FUN.VALUE = 1, USE.NAMES = FALSE), length(y) * p)
})

test_that("stratified splitting is balanced for integer n*p within stratum", {
  y <- rep(LETTERS[1:10], each = 10)
  p <- c(0.6, 0.2, 0.2)
  out <- partition(y, p, type = "stratified")
  expect_equal(length(unique(table(y[out[[1]]]))), 1)
})

test_that("argument 'split_into_list' works", {
  y <- rep(LETTERS[1:10], each = 10)
  p <- c(0.6, 0.2, 0.2)
  expect_true(is.list(partition(y, p, split_into_list = TRUE)))
  expect_false(is.list(partition(y, p, split_into_list = FALSE)))
})

test_that("argument 'seed' works", {
  y <- rep(LETTERS[1:10], each = 10)
  p <- c(0.6, 0.2, 0.2)
  expect_equal(partition(y, p, seed = 1), partition(y, p, seed = 1))
})

test_that("argument 'use_names' works", {
  y <- rep(LETTERS[1:10], each = 10)
  p <- c(0.6, 0.2, 0.2)
  expect_equal(names(partition(y, p, use_names = TRUE)),
               names(partition(y, p, use_names = FALSE)))
  names(p) <- c("a", "b", "c")
  expect_named(partition(y, p, use_names = TRUE), c("a", "b", "c"))
  expect_named(partition(y, p, use_names = FALSE), c("1", "2", "3"))
})

test_that("blocked splitting works", {
  y <- rep(LETTERS[1:10], each = 10)
  p <- c(0.5, 0.5)
  out <- partition(y, p, type = "blocked")
  expect_equal(out[[1]], 1:50)
  expect_equal(out[[2]], 51:100)
})

test_that("grouped splitting works", {
  y <- rep(LETTERS[1:2], each = 50)
  p <- c(0.5, 0.5)
  out <- partition(y, p, type = "grouped")
  expect_true(all(out[[1]] == 1:50) || all(out[[2]] == 1:50))
})

test_that("basic splitting works with seed", {
  y <- rep(LETTERS[1:10], each = 5)
  p <- c(0.5, 0.5)
  out <- partition(y, p, type = "basic", seed = 1)
  expect_equal(out[[1]][1:7], c(1, 3, 5, 7, 8, 10, 12))
})

test_that("stratified splitting works with seed", {
  y <- rep(LETTERS[1:10], each = 5)
  p <- c(0.5, 0.5)
  out <- partition(y, p, type = "stratified", seed = 1)
  expect_equal(out[[1]][1:7], c(1, 3, 5, 7, 8, 10, 11))
})

test_that("grouped splitting works with seed", {
  y <- rep(LETTERS[1:10], each = 5)
  p <- c(0.5, 0.5)
  out <- partition(y, p, type = "grouped", seed = 1)
  expect_equal(out[[1]][1:10], c(6:10, 16:20))
})

test_that("character and factor input give same result", {
  y <- rep(LETTERS[1:10], each = 5)
  p <- c(0.5, 0.5)
  out1 <- partition(y, p, type = "grouped", seed = 1)
  out2 <- partition(factor(y), p, type = "grouped", seed = 1)
  expect_equal(out1, out2)
})

test_that("stratified splitting on continous data works", {
  y <- seq(0, 1, by = 0.01)
  p <- c(0.5, 0.5)
  out <- partition(y, p, n_bins = 2, seed = 1)
  expect_lt(abs(diff(sapply(out, mean))), 1)
})

test_that("stratified splitting on continous data works with missing values", {
  y <- c(1:99, NA)
  p <- c(0.5, 0.5)
  out <- partition(y, p, n_bins = 2, seed = 1)
  expect_equal(length(unlist(out)), length(y))
})

test_that("stratified splitting on continous data reacts on n_bins", {
  y <- rep(1:10, each = 10)
  p <- c(0.5, 0.5)
  out1 <- partition(y, p, n_bins = 2, seed = 1)
  out2 <- partition(y, p, n_bins = 11, seed = 1)
  expect_equal(isTRUE(all.equal(out1, out2)), FALSE)
})

test_that("tiny data sets are providing non-empty partitions for stratified sampling", {
  y <- c("A", "A", "B")
  expect_message(partition(y, p = c(0.9, 0.1), seed = 1))
  expect_message(partition(y, p = c(0.9, 0.1, 0.2), seed = 10))
})

test_that("tiny data sets are providing non-empty partitions for basic sampling", {
  y <- c("A", "A", "B")
  expect_message(partition(y, p = c(0.9, 0.1), seed = 2, type = "basic"))
  expect_message(partition(y, p = c(0.9, 0.1, 0.2), seed = 10, type = "basic"))
})

test_that("tiny data sets are providing non-empty partitions for grouped sampling", {
  y <- c("A", "A", "B")
  expect_message(partition(y, p = c(0.9, 0.1), seed = 1, type = "grouped"))
})



