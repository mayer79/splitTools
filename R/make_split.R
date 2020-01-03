#' Split data
#'
#' This function provides row numbers per data split, see examples, e.g. into training/validation/test. Different types of splits are supported (basic, stratified, or grouped).
#'
#' Note that for grouped splits, it might happen that some groups are dropped by chance.
#' @importFrom stats ave quantile
#' @param y Either the variable used for stratification or grouping.
#' @param p A vector with split probabilities, e.g. c(train = 0.7, valid = 0.3).
#' @param type Split type. One of "stratified", "basic", "grouped".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param invert Set to \code{TRUE} if the row numbers not in the split are to be returned. Defaults to \code{FALSE}.
#' @param seed Integer random seed.
#' @return A list with row numbers.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' make_split(y, p = c(0.7, 0.3))
#' make_split(y, p = c(train = 0.8, valid = 0.1, test = 0.1))
#' make_split(rep(c("a", "b"), each = 10), p = c(0.7, 0.3), type = "grouped", seed = 30)
make_split <- function(y, p, type = c("stratified", "basic", "grouped"),
                       n_bins = 10, invert = FALSE, seed = NULL) {
  type <- match.arg(type)
  stopifnot(length(p) > 0L, sum(p) == 1, p >= 0)
  p_nms <- names(p)
  if (!is.null(p_nms)) {
    p_set <- factor(p_nms, levels = p_nms)
  } else {
    p_set <- seq_along(p)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ind <- seq_along(y)
  smp_fun <- function(z) {
    sample(p_set, length(z), replace = TRUE, prob = p)
  }
  if (type == "basic") {
    out <- split(ind, smp_fun(y))
  } else if (type == "stratified") {
    if (is.numeric(y) && length(unique(y)) > n_bins) {
      y <- .bin(y, n_bins)
    }
    out <- split(ind, ave(ind, y, FUN = smp_fun))
  } else if (type == "grouped") {
    y_unique <- unique(y)
    if (length(p) > (m <- length(y_unique))) {
      stop("`length(p)` should not be larger than ", m)
    }
    y_folds <- smp_fun(y_unique)
    out <- split(ind, y_folds[match(y, y_unique)])
  }
  if (invert) lapply(out, function(z) ind[-z]) else out
}

# Little helpers
.bin <- function(y, n_bins) {
  qu <- unique(quantile(y, seq(0, 1, length.out = n_bins)))
  findInterval(y, qu, rightmost.closed = TRUE)
}
