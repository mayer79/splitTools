#' fast_shuffle
#'
#' Fast shuffling of vector.
#'
#' @param x An integer vector
#'
#' @return xxx
#' @useDynLib splitTools
#' @importFrom Rcpp sourceCpp
#' @export
fast_shuffle <- function(x, n = length(x)) {
  C_fast_shuffle(x, n = n)
}
