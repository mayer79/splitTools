#' Creates Folds
#'
#' This function provides a list of row numbers per fold of k-fold cross-validation (basic, stratified, or grouped). Repeated fold creation is supported as well.
#' @param y Either the variable used for stratification or grouping.
#' @param k Number of folds.
#' @param type Split type. One of "stratified", "basic", "grouped".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param m_rep How many times should the data be split into k folds? Default is 1, i.e. no repeated folds.
#' @param use_names Should folds be named?
#' @param return_out Set to \code{TRUE} if the row numbers not in the fold are to be returned. Defaults to \code{FALSE}.
#' @param seed Integer random seed.
#' @return A list with row numbers per fold.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' create_folds(y)
#' create_folds(y, k = 2)
#' create_folds(y, k = 2, m_rep = 2)
create_folds <- function(y, k = 5, type = c("stratified", "basic", "grouped"),
                         n_bins = 10, m_rep = 1, use_names = TRUE,
                         return_out = FALSE, seed = NULL) {
  # Initial checks
  type <- match.arg(type)
  stopifnot(is.atomic(y),
            length(y) > 0L,
            k > 0L,
            m_rep > 0L)

  # Initializations
  if (!is.null(seed)) {
    set.seed(seed)
  }
  p <- rep(1 / k, times = k)
  if (use_names) {
    sfx <- .names("Rep", seq_len(m_rep))
  }
  f <- function(i = 1) {
    res <- make_split(y = y, p = p, type = type, n_bins = n_bins,
                      invert = !return_out && k > 2)
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

  # Call make_split
  if (m_rep == 1) f() else unlist(lapply(seq_len(m_rep), f), recursive = FALSE)
}

# Little helper(s)
.names <- function(prefix, suffix) {
  paste0(prefix, gsub(" ", "0", format(suffix)))
}

