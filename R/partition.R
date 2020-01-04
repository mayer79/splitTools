#' Split Data into Partitions
#'
#' This function provides a list of row indices per partition, e.g. into training/validation/test. Different types of splits are supported (basic, stratified, or grouped), see Details.
#'
#' By default, the function does stratified partitions. This will approximately balance the partitions regarding the distribution of the input vector \code{y}. For numeric input, it is first binned into \code{n_bins} quantile groups. If \code{type = "grouped"}, groups specified by \code{y} are kept together when splitting. This is relevant for clustered or panel data. Note that for grouped splits, if only very few groups exist, some groups might be dropped by chance.
#' @importFrom stats ave quantile
#' @param y Either the variable used for "stratification" or "grouped" splits. For "basic" splits, any vector of the same length as the data intended to split.
#' @param p A vector with split probabilities, e.g. c(train = 0.7, valid = 0.3). If named, names are passed to the output list elements.
#' @param type Split type. One of "stratified", "basic", "grouped". The default is "stratified".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param invert Set to \code{TRUE} if the row numbers not in the split are to be returned. Defaults to \code{FALSE}.
#' @param seed Integer random seed.
#' @return A list with row indices per partition.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' partition(y, p = c(0.7, 0.3))
#' partition(y, p = c(train = 0.8, valid = 0.1, test = 0.1))
#' partition(rep(c("a", "b"), each = 10), p = c(0.7, 0.3), type = "grouped", seed = 30)
partition <- function(y, p, type = c("stratified", "basic", "grouped"),
                       n_bins = 10, invert = FALSE, seed = NULL) {
  # Input checks
  type <- match.arg(type)
  stopifnot(length(p) >= 1L, sum(p) == 1, p >= 0,
            is.atomic(y), length(y) >= 2L)

  # Initializations
  if (inherits(y, "Surv")) {
    y <- y[, 2]
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ind <- seq_along(y)

    # Calculation of indices per split
  if (type == "basic") {
    out <- split(ind, .smp_fun(y, p))
  } else if (type == "stratified") {
    if (is.numeric(y) && length(unique(y)) > n_bins) {
      y <- .bin(y, n_bins)
    }
    out <- split(ind, ave(ind, y, FUN = function(z) .smp_fun(z, p)))
  } else if (type == "grouped") {
    y_unique <- unique(y)
    stopifnot(length(p) <= length(y_unique))
    y_folds <- .smp_fun(y_unique, p)
    out <- split(ind, y_folds[match(y, y_unique)])
  }

  # Correct naming of partitions
  if (!is.null(names(p))) {
    names(out) <- names(p)[as.integer(names(out))]
  }

  # Inversion and output
  if (invert) lapply(out, function(z) ind[-z]) else out
}

# Little helper(s)
.bin <- function(y, n_bins) {
  qu <- unique(quantile(y, seq(0, 1, length.out = n_bins)))
  findInterval(y, qu, rightmost.closed = TRUE)
}

.smp_fun <- function(z, p) {
  sample(seq_along(p), length(z), replace = TRUE, prob = p)
}
