set.seed(2)
y_multi <- data.frame(
  A = rep(c(letters[1:4]), each = 100),
  B = factor(sample(c(0, 1), 400, replace = TRUE)),
  C = rnorm(400)
)

test_that("k = 1 raises an error", {
  expect_error(multi_strata(y_multi, k = 1))
})

test_that("Any strategy produces a factor", {
  expect_true(is.factor(multi_strata(y_multi, strategy = "kmeans", k = 4)))
  expect_true(is.factor(multi_strata(y_multi, strategy = "interaction", k = 4)))
})
