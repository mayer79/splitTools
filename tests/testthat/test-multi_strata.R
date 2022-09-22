test_that("calculate strata based on multiple criteria", {
  set.seed(2)
  y_multi <- data.frame(
    rep(c(letters[1:4]), each = 100),
    factor(sample(c(0, 1), 400, replace = TRUE)),
    rnorm(400)
  )
  expect_message(
    multi_strata(y_multi, num_cat = 3),
    regexp = "different groups"
  )

  expect_error(
    multi_strata(y_multi, num_cat = 1),
    regexp = "num_cat > 1L is not TRUE"
  )

  y_multi[, 2] <- as.numeric(as.character(y_multi[, 2]))
  expect_error(
    multi_strata(y_multi, num_cat = 5),
    regexp = "reduce the number"
  )

  set.seed(2)
  y_multi <- data.frame(
    as.logical(sample(c(0, 1), 400, replace = TRUE)),
    as.logical(sample(c(0, 1), 400, replace = TRUE)),
    rnorm(400)
  )

  expect_error(
    multi_strata(y_multi, num_cat = 3),
    regexp = "Not enough columns"
  )
})
