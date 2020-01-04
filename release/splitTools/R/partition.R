#' Split Data into Partitions
#'
#' This function provides row indices for data splitting, e.g. into training, validation, and test. Different types of split strategies are supported ("basic", "stratified" (the default), or "grouped"), see Details. The partition indices are either returned as a list with one element per partition (the default) or as vector of partition ids.
#'
#' By default, the function uses stratified splitting. This will balance the partitions as good as possible regarding the distribution of the input vector \code{y}. Numeric input is first binned into \code{n_bins} quantile groups. If \code{type = "grouped"}, groups specified by \code{y} are kept together when splitting. This is relevant for clustered or panel data.
#' @importFrom stats ave quantile
#' @param y Either the variable used for "stratification" or "grouped" splits. For "basic" splits, any vector of the same length as the data intended to split.
#' @param p A vector with split probabilities, e.g. c(train = 0.7, valid = 0.3). Names are passed to the output.
#' @param type Split type. One of "stratified", "basic", "grouped". The default is "stratified".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param split_into_list Should the resulting partition vector be split into a list or not? Default is\code{TRUE}.
#' @param use_names Should names of \code{p} be used as partition names? Default is \code{TRUE}.
#' @param seed Integer random seed.
#' @return A list with row indices per partition (if \code{split_into_list = TRUE}) or a vector of partition ids.
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
partition <- function(y, p, type = c("stratified", "basic", "grouped"), n_bins = 10,
                      split_into_list = TRUE, use_names = TRUE, seed = NULL) {
  # Input checks
  type <- match.arg(type)
  stopifnot(length(p) >= 1L, p > 0,
            is.atomic(y),
            (n <- length(y)) >= 2L)

  # Initializations
  p <- p / sum(p)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Calculation of partition ids
  if (type == "basic") {
    out <- .smp_fun(n, p)
  } else if (type == "stratified") {
    if (is.numeric(y) && length(unique(y)) > n_bins) {
      y <- .bin(y, n_bins)
    }
    out <- ave(integer(n), y, FUN = function(z) .smp_fun(length(z), p))
  } else if (type == "grouped") {
    y_unique <- unique(y)
    m <- length(y_unique)
    stopifnot(length(p) <= m)
    y_folds <- .smp_fun(m, p)
    out <- y_folds[match(y, y_unique)]
  }

  # Output
  if (use_names && !is.null(names(p))) {
    out <- factor(out, levels = seq_along(p), labels = names(p))
  }
  if (split_into_list) split(seq_along(y), out) else out
}

# Little helper(s)
.bin <- function(y, n_bins) {
  qu <- unique(quantile(y, seq(0, 1, length.out = n_bins)))
  findInterval(y, qu, rightmost.closed = TRUE)
}

# .smp_fun <- function(z, p) {
#   sample(seq_along(p), length(z), replace = TRUE, prob = p)
# }

.smp_fun <- function(n, p) {
  sample(rep.int(seq_along(p), times = ceiling(p * n)), n)
}
