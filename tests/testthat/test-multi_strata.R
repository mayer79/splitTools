test_that("patition based on multiple criteria", {
  set.seed(2)
  y_multi <- data.frame(
    rep(c(letters[1:4]), each = 100),
    factor(sample(c(0, 1), 400, replace = TRUE)),
    rnorm(400)
  )
  expect_message(
    multi_strata(y_multi, num_cat = 3),
    regexp = "strata"
  )

  expect_error(
    multi_strata(y_multi, num_cat = 1)
  )

  y_multi[, 2] <- as.numeric(as.character(y_multi[, 2]))
  expect_error(
    multi_strata(y_multi, num_cat = 5)
  )
})
