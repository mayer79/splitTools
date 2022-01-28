test_that("number of folds is correct", {
  y <- rep(LETTERS[1:10], each = 10)
  out <- create_timefolds(y, k = 3)
  expect_equal(length(out), 3L)
})

test_that("argument 'use_names' works", {
  y <- rep(LETTERS[1:10], each = 10)
  expect_named(create_timefolds(y, k = 3, use_names = TRUE), c("Fold1", "Fold2", "Fold3"))
  expect_named(create_timefolds(y, k = 3, use_names = FALSE), NULL)
})

test_that("character and factor input give same result", {
  y <- rep(LETTERS[1:10], each = 5)
  k <- 3
  out1 <- create_timefolds(y, k = k)
  out2 <- create_timefolds(factor(y), k = k)
  expect_equal(out1, out2)
})

test_that("missing values are okay", {
  y <- c(1:99, NA)
  out <- create_timefolds(y, k = 2)
  expect_equal(length(unlist(out[[2]])), length(y))
})

test_that("Moving windows work", {
  y <- 1:100
  k <- 4
  out <- create_timefolds(y, k = k, type = "moving")
  expect_equal(out$Fold4$insample, 61:80)
  expect_equal(out$Fold4$outsample, 81:100)
})

test_that("Extending windows work", {
  y <- 1:100
  k <- 4
  out <- create_timefolds(y, k = k, type = "extending")
  expect_equal(out$Fold4$insample, 1:80)
  expect_equal(out$Fold4$outsample, 81:100)
})
