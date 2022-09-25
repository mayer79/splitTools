#' Create Strata from Multiple Features
#'
#' This function is a helper function to create strata based on multiple criteria,
#' i.e. the columns of a data.frame, which can then be used as input variable
#' to the splitting functions. Currently, the function offers two strategies to create
#' the strata, see Details.
#'
#' The default \code{strategy} to turn the columns of the data.frame into a one-dimensional
#' stratification factor is "kmeans": It selects all numeric, logical, character, and
#' factor columns, transforms them into a numeric matrix (ordered factors to integer,
#' unordered factors or categoricals one-hot encoded), scales them and then runs a
#' k-means cluster analysis aiming at \code{k} clusters. The \code{stategy} "interaction"
#' selects the same columns. Then, it categorizes all numeric columns into approximately
#' \code{k} bins. Then, all combinations of all columns are formed by calling
#' \code{base::interaction()}.
#'
#' @param df A data.frame. All columns of the provided data.frame are used to
#' compute strata using the defined \code{strategy} (see Details).
#' @param strategy A character.
#' The strategy (either "kmeans" or "interaction") to compute the strata (see Details).
#' @param k An integer. For \code{strategy = "kmeans"}, it is the desired number of strata,
#' while for \code{strategy = "interaction"}, it is the approximate number of bins per
#' numeric feature before forming all combinations.
#'
#' @return A vector containing the strata as a factor that can be passed further on to
#' the splitting functions.
#'
#' @seealso \code{\link{partition}}, \code{\link{create_folds}}.
#'
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
# HELPER FUNCTION
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
