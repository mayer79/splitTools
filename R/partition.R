#' Split Data into Partitions
#'
#' This function provides row indices for data splitting, e.g., to split data
#' into training, validation, and test. Different types of split strategies are
#' supported, see Details.
#' The partition indices are either returned as list with one element per partition
#' (the default) or as vector of partition IDs.
#'
#' By default, the function uses stratified splitting. This will balance the partitions
#' as good as possible regarding the distribution of the input vector `y`.
#' (Numeric input is first binned into `n_bins` quantile groups.)
#' If `type = "grouped"`, groups specified by `y` are kept together when
#' splitting. This is relevant for clustered or panel data.
#' In contrast to basic splitting, `type = "blocked"` does not sample indices
#' at random, but rather keeps them in groups: e.g., the first 80% of observations form
#' a training set and the remaining 20% are used for testing.
#'
#' @param y Either the variable used for "stratification" or "grouped" splits.
#'   For other types of splits, any vector of the same length as the data
#'   intended to split.
#' @param p A vector with split probabilities per partition, e.g.,
#'   `c(train = 0.7, valid = 0.3)`. Names are passed to the output.
#' @param type Split type. One of "stratified" (default), "basic", "grouped", "blocked".
#' @param n_bins Approximate numbers of bins for numeric `y`
#'   (only for `type = "stratified"`).
#' @param split_into_list Should the resulting partition vector be split into a list?
#'   Default is `TRUE`.
#' @param use_names Should names of `p` be used as partition names?
#'   Default is `TRUE`.
#' @param shuffle Should row indices be randomly shuffled within partition?
#'   Default is `FALSE`. Shuffling is only possible when `split_into_list = TRUE`.
#' @param seed Integer random seed.
#' @returns
#'   A list with row indices per partition (if `split_into_list = TRUE`)
#'   or a vector of partition IDs.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' partition(y, p = c(0.7, 0.3), seed = 1)
#' partition(y, p = c(0.7, 0.3), split_into_list = FALSE, seed = 1)
#' p <- c(train = 0.8, valid = 0.1, test = 0.1)
#' partition(y, p, seed = 1)
#' partition(y, p, split_into_list = FALSE, seed = 1)
#' partition(y, p, split_into_list = FALSE, use_names = FALSE, seed = 1)
#' partition(y, p = c(0.7, 0.3), type = "grouped")
#' partition(y, p = c(0.7, 0.3), type = "blocked")
#' @seealso [create_folds()]
partition <- function(y, p,
                      type = c("stratified", "basic", "grouped", "blocked"),
                      n_bins = 10L, split_into_list = TRUE, use_names = TRUE,
                      shuffle = FALSE, seed = NULL) {
  # Input checks
  type <- match.arg(type)
  stopifnot(
    length(p) >= 1L,
    p > 0,
    is.atomic(y),
    (n <- length(y)) >= 2L
  )

  # Initializations
  p <- p / sum(p)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Calculation of partition ids
  if (type == "basic") {
    out <- .smp_fun(n, p)
    out <- .fill_empty_partitions(out, p = p)
  } else if (type == "blocked") {
    out <- rep.int(seq_along(p), times = ceiling(p * n))[seq_len(n)]
  } else if (type == "stratified") {
    if (is.numeric(y) && length(unique(y)) > n_bins) {
      y <- .bin(y, n_bins)
    }
    if (anyNA(y)) {
      y <- factor(y, exclude = NULL)
    }
    out <- stats::ave(integer(n), y, FUN = function(z) .smp_fun(length(z), p))
    out <- .fill_empty_partitions(out, p = p)
  } else if (type == "grouped") {
    y_unique <- unique(y)
    m <- length(y_unique)
    stopifnot(length(p) <= m)
    y_folds <- .smp_fun(m, p)
    y_folds <- .fill_empty_partitions(y_folds, p = p)
    out <- y_folds[match(y, y_unique)]
  }

  # Output
  if (use_names && !is.null(names(p))) {
    out <- factor(out, levels = seq_along(p), labels = names(p))
  }
  if (!split_into_list) {
    if (shuffle) {
      message("Shuffling has no effect with split_into_list = TRUE.")
    }
    return(out)
  }
  out <- split(seq_along(y), out)
  if (shuffle) {
    out <- lapply(out, .shuffle)
  }
  out
}

# Little helpers

# Save shuffling (even if x has length 1)
.shuffle <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

# Efficient binning
.bin <- function(y, n_bins) {
  qu <- stats::quantile(y, seq(0, 1, length.out = n_bins + 1L), na.rm = TRUE)
  findInterval(y, unique(qu), rightmost.closed = TRUE)
}

# This this secret heart of splitTools
.smp_fun <- function(n, p) {
  sample(rep.int(seq_along(p), times = ceiling(p * n)), n)
}

# Fills empty partitions
.fill_empty_partitions <- function(fi, p) {
  counts <- tabulate(fi, nbins = length(p))
  empty <- which(counts == 0L)
  n_empty <- length(empty)

  if (n_empty == 0L) {
    return(fi)
  }

  message("Empty partition detected. Redistributing...")

  # Find positions of potential donors
  drop_random <- function(z) {
    m <- length(z) - 1L
    if (m >= 1L) sample(z, m)
  }
  positions <- split(seq_along(fi), fi)
  donors <- unlist(lapply(positions, drop_random), use.names = FALSE)
  n_donors <- length(donors)

  # Randomly select donors
  if (n_empty > n_donors) {
    message("Cannot fill all empty partitions.")
    n_empty <- n_donors
  }
  selected <- donors[sample.int(n_donors, n_empty)]

  # Replace donors by empty partition numbers
  fi[selected] <- empty[seq_len(n_empty)]
  fi
}
