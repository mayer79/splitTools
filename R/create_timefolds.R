#' Creates Folds for Time Series Data
#'
#' This function provides a list with in- and out-of-sample indices per fold used for time series k-fold cross-validation, see Details.
#'
#' The data is first partitioned into k+1 sequential blocks B_1 to B_{k+1}. Each fold consists of two index vectors: one with in-sample row numbers, the other with out-of-sample row numbers. The first fold uses B_1 as in-sample and B_2 as out-of-sample data. The second one uses {B_1, B_2} as in-sample and B_3 as out-of-sample data etc. until the kth fold with {B_1, ..., B_k} as in-sample and B_{k+1} as out-of-sample data. This makes sure that out-of-sample data always follows in-sample data.
#'
#' @param y Any vector of the same length as the data intended to split.
#' @param k Number of folds.
#' @param use_names Should folds be named? Default is \code{TRUE}.
#' @return A nested list with in-sample ("insample") and out-of-sample ("outsample") indices per fold.
#' @export
#' @examples
#' y <- runif(100)
#' create_timefolds(y)
#' create_timefolds(y, use_names = FALSE)
create_timefolds <- function(y, k = 5, use_names = TRUE) {
  # Input checks
  stopifnot(is.atomic(y), length(y) >= 2L, k >= 2)

  # Blocked partition of k + 1 blocks
  res <- partition(y = y,
                   p = rep(1 / (k + 1), times = k + 1),
                   type = "blocked",
                   use_names = FALSE)

  # Create k in-/outsamples
  res <- lapply(res[-1], .make_in_outsample)

  # Organize output
  if (use_names) {
    # .names defined in "create_folds.R"
    names(res) <- .names("Fold", seq_along(res))
  } else {
    res <- unname(res)
  }
  res
}

# Helper function
.make_in_outsample <- function(z) {
  list(insample = seq_len(z[1] - 1), outsample = z)
}

