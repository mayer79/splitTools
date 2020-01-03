#' Creates Folds
#'
#' This function provides a list of row numbers per fold of k-fold cross-validation (basic, stratified, or grouped). Repeated fold creation is supported as well.
#' @param y Either the variable used for stratification or grouping.
#' @param k Number of folds.
#' @param type Split type. One of "stratified", "basic", "grouped".
#' @param m_repetitions How many times should the data be split into k folds? Default is 1, i.e. no repeated folds.
#' @param names Should folds be named?
#' @param return_out Set to \code{TRUE} if the row numbers not in the fold are to be returned. Defaults to \code{FALSE}.
#' @param seed Integer random seed.
#' @return A list with row numbers per fold.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' create_folds(y)
#' create_folds(y, k = 2)
#' create_folds(y, k = 2, m_repetitions = 2)
create_folds <- function(y, k = 5, type = c("stratified", "basic", "grouped"), 
                         m_repetitions = 1, names = TRUE, 
                         return_out = FALSE, seed = NULL) {
  type <- match.arg(type)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  f <- function() make_split(y = y, p = rep(1 / k, times = k), 
                             type = type, invert = !return_out)
  if (m_repetitions == 1) {
    out <- unname(f())
  } else {
    out <- unlist(replicate(m_repetitions, f(), simplify = FALSE), 
                  recursive = FALSE, use.names = FALSE)
  }
  if (names) {
    names(out) <- paste0("Fold", gsub(" ", "0", format(seq_along(out))))
  }
  out
}

