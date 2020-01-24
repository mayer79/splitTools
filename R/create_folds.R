#' Creates Folds
#'
#' This function provides a list of row indices per fold of k-fold cross-validation (basic, stratified, grouped, or blocked). Repeated fold creation is supported as well.
#'
#' By default, the function uses stratified splitting. This will balance the folds regarding the distribution of the input vector \code{y}. Numeric input is first binned into \code{n_bins} quantile groups. If \code{type = "grouped"}, groups specified by \code{y} are kept together when splitting. This is relevant for clustered or panel data. In contrast to basic splitting, \code{type = "blocked"} does not sample indices at random, but rather keeps them in sequential groups.
#' @param y Either the variable used for "stratification" or "grouped" splits. For other types of splits, any vector of the same length as the data intended to split.
#' @param k Number of folds.
#' @param type Split type. One of "stratified", "basic", "grouped", "blocked". The default is "stratified".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param m_rep How many times should the data be split into k folds? Default is 1, i.e. no repetitions.
#' @param use_names Should folds be named? Default is \code{TRUE}.
#' @param invert Set to \code{TRUE} if the row numbers not in the fold are to be returned. Default is \code{FALSE}.
#' @param seed Integer random seed.
#' @return A list with row indices per fold.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' create_folds(y)
#' create_folds(y, k = 2)
#' create_folds(y, k = 2, m_rep = 2)
#' create_folds(y, k = 3, type = "blocked")
create_folds <- function(y, k = 5, type = c("stratified", "basic", "grouped", "blocked"),
                         n_bins = 10, m_rep = 1, use_names = TRUE,
                         invert = FALSE, seed = NULL) {
  # Input checks
  type <- match.arg(type)
  stopifnot(is.atomic(y), length(y) >= 2L,
            k >= 2,
            m_rep >= 1)
  if (type == "blocked" && m_rep > 1) {
    message("Repeated blocked cross-validation will return the same indices for each repetition. Setting m_rep = 1...")
    m_rep <- 1
  }

  # Initializations
  if (!is.null(seed)) {
    set.seed(seed)
  }
  p <- rep(1 / k, times = k)
  if (use_names) {
    sfx <- .names("Rep", seq_len(m_rep))
  }
  f <- function(i = 1) {
    res <- partition(y = y, p = p, type = type, n_bins = n_bins,
                     split_into_list = TRUE, use_names = FALSE)
    res <- if (invert) res else lapply(res, function(z) seq_along(y)[-z])
    if (use_names) {
      names(res) <- .names("Fold", seq_along(res))
      if (m_rep > 1) {
        names(res) <- paste(names(res), sfx[i], sep = ".")
      }
    } else {
      res <- unname(res)
    }
    res
  }

  # Call partition once or multiple times
  if (m_rep == 1) f() else unlist(lapply(seq_len(m_rep), f), recursive = FALSE)
}

# Little helper(s)
.names <- function(prefix, suffix) {
  paste0(prefix, gsub(" ", "0", format(suffix)))
}

