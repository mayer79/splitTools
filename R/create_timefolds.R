#' Creates Folds for Time Series Data
#'
#' This function provides a list with in- and out-of-sample indices per fold used for time series k-fold cross-validation, see Details.
#'
#' The data is first partitioned into k+1 sequential blocks B_1 to B_{k+1}. Each fold consists of two index vectors: one with in-sample row numbers, the other with out-of-sample row numbers. The first fold uses B_1 as in-sample and B_2 as out-of-sample data. The second one uses either B_2 (if \code{type = "extending"}) or {B_1, B_2} (if \code{type = "moving"}) as in-sample and B_3 as out-of-sample data etc. until the kth fold with {B_1, ..., B_k} ("extending") resp. B_k ("moving") as in-sample and B_{k+1} as out-of-sample data. This makes sure that out-of-sample data always follows in-sample data.
#'
#' @importFrom stats setNames
#' @param y Any vector of the same length as the data intended to split.
#' @param k Number of folds.
#' @param use_names Should folds be named? Default is \code{TRUE}.
#' @param type Should in-sample data be "extending" over the folds (default) or consist of one single fold ("moving")?
#' @return A nested list with in-sample ("insample") and out-of-sample ("outsample") indices per fold.
#' @export
#' @examples
#' y <- runif(100)
#' create_timefolds(y)
#' create_timefolds(y, use_names = FALSE)
#' create_timefolds(y, use_names = FALSE, type = "moving")
create_timefolds <- function(y, k = 5, use_names = TRUE, type = c("extending", "moving")) {
  # Input checks
  type <- match.arg(type)
  stopifnot(is.atomic(y), length(y) >= 2L, k >= 2)

  # Blocked partition of k + 1 blocks
  res <- partition(y = y,
                   p = rep(1 / (k + 1), times = k + 1),
                   type = "blocked",
                   use_names = FALSE)

  # Create k in-/outsamples for given type
  if (type == "extending") {
    res <- lapply(res[-1], .make_in_outsample_e)
  } else if (type == "moving") {
    res <- lapply(seq_len(k), .make_in_outsample_m, res)
  }

  # Organize output
  if (use_names) {
    # .names defined in "create_folds.R"
    names(res) <- .names("Fold", seq_along(res))
  } else {
    res <- unname(res)
  }
  res
}

# Helper functions
.make_in_outsample_e <- function(z) {
  list(insample = seq_len(z[1] - 1), outsample = z)
}

.make_in_outsample_m <- function(i, res) {
  setNames(res[c(i, i + 1)], c("insample", "outsample"))
}

