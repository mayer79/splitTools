#' Create Strata from Multiple Features
#'
#' Creates a stratification vector based on multiple columns of
#' a data.frame that can then be passed to the splitting functions.
#' Currently, the function offers two strategies: "kmeans" runs a k-means
#' cluster analysis on scaled input. (Ordered factors are integer encoded first,
#' unordered factors and character columns are one-hot-encoded.)
#' The second `stategy = "interaction"` creates all feature value
#' combinations (after binning numeric columns into approximately `k` bins).
#'
#' @param df A `data.frame` used to form the stratification vector.
#' @param strategy A string (either "kmeans" or "interaction") to compute the strata,
#'   see description.
#' @param k An integer. For `strategy = "kmeans"`, it is the desired number of strata,
#'   while for `strategy = "interaction"`, it is the approximate number of bins per
#'   numeric feature before forming all combinations.
#' @returns Factor with strata as levels.
#' @seealso [partition()], [create_folds()]
#' @export
#' @examples
#' y_multi <- data.frame(
#'   A = rep(c(letters[1:4]), each = 20),
#'   B = factor(sample(c(0, 1), 80, replace = TRUE)),
#'   c = rnorm(80)
#' )
#' y <- multi_strata(y_multi, k = 3)
#' folds <- create_folds(y, k = 5)
multi_strata <- function(df, strategy = c("kmeans", "interaction"), k = 3L) {
  strategy <- match.arg(strategy)
  stopifnot(is.data.frame(df), k >= 2L, k <= nrow(df))
  FUN <- switch(strategy, "kmeans" = .kmeans, "interaction" = .interaction)
  FUN(.good_cols(df), k = k)
}

#=======================
# HELPER FUNCTIONS
#=======================

# Strategy: kmeans
.kmeans <- function(df, k) {
  # Treat ordered as numeric
  v <- colnames(df)[vapply(df, is.ordered, FUN.VALUE = logical(1L))]
  if (length(v) >= 1L) {
    df[v] <- lapply(df[v], as.integer)
  }

  # Now the real work
  df <- scale(stats::model.matrix(~ . + 0, data = df))
  factor(stats::kmeans(df, centers = k)$cluster)
}

# Strategy: interactions across all reasonable columns
.interaction <- function(df, k) {
  v <- colnames(df)[vapply(df, is.numeric, FUN.VALUE = logical(1L))]
  if (length(v) >= 1L) {
    df[v] <- lapply(df[v], .bin_pretty, n_bins = k)
  }
  interaction(df, drop = TRUE, sep = ":")
}

# Select reasonable columns
.good_cols <- function(x) {
  ok <- vapply(
    x,
    function(v)
      is.factor(v) || is.character(v) || is.numeric(v) || is.logical(v),
    FUN.VALUE = logical(1L)
  )
  v <- colnames(x)[ok]
  if (length(v) == 0L) {
    stop("No numeric, factor, character or logical columns in data")
  }
  x[v]
}

# Cuts x into quantile groups (like .bin, but with nice labels)
.bin_pretty <- function(x, n_bins) {
  # +1 required as e.g. cutting a vector with breaks of length 5 results in
  # 4 groups and users define with 'num_cat' the number of categories
  # for numeric variables
  probs <- seq(0, 1, len = n_bins + 1L)
  breaks <- unique(stats::quantile(x, probs = probs, names = FALSE))
  cut(x, breaks, include.lowest = TRUE)
}
