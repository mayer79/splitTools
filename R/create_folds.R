#' Create Folds
#'
#' This function provides a list of row indices used for k-fold cross-validation
#' (basic, stratified, grouped, or blocked). Repeated fold creation is supported as well.
#' By default, in-sample indices are returned.
#'
#' By default, the function uses stratified splitting. This will balance the folds
#' regarding the distribution of the input vector `y`.
#' (Numeric input is first binned into `n_bins` quantile groups.)
#' If `type = "grouped"`, groups specified by `y` are kept together
#' when splitting. This is relevant for clustered or panel data.
#' In contrast to basic splitting, `type = "blocked"` does not sample
#' indices at random, but rather keeps them in sequential groups.
#'
#' @inheritParams partition
#' @param k Number of folds.
#' @param m_rep How many times should the data be split into k folds?
#'   Default is 1, i.e., no repetitions.
#' @param use_names Should folds be named? Default is `TRUE`.
#' @param invert Set to `TRUE` in order to receive out-of-sample indices.
#'   Default is `FALSE`, i.e., in-sample indices are returned.
#' @param shuffle Should row indices be randomly shuffled within folds?
#'   Default is `FALSE`.
#' @returns If `invert = FALSE` (the default), a list with in-sample row indices.
#'   If `invert = TRUE`, a list with out-of-sample indices.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' create_folds(y)
#' create_folds(y, k = 2)
#' create_folds(y, k = 2, m_rep = 2)
#' create_folds(y, k = 3, type = "blocked")
#' @seealso [partition()], [create_timefolds()]
create_folds <- function(y, k = 5L,
                         type = c("stratified", "basic", "grouped", "blocked"),
                         n_bins = 10L, m_rep = 1L, use_names = TRUE,
                         invert = FALSE, shuffle = FALSE, seed = NULL) {
  # Input checks
  type <- match.arg(type)
  stopifnot(
    is.atomic(y),
    length(y) >= 2L,
    k >= 2L,
    m_rep >= 1L
  )
  if (type == "blocked" && m_rep > 1L) {
    message("Repeated blocked cross-validation will return the same indices for each repetition. Setting m_rep = 1...")
    m_rep <- 1L
  }

  # Initializations
  if (!is.null(seed)) {
    set.seed(seed)
  }
  p <- rep(1 / k, times = k)
  if (use_names && m_rep > 1L) {
    sfx <- .names("Rep", seq_len(m_rep))
  }
  f <- function(i = 1L) {
    res <- partition(
      y = y,
      p = p,
      type = type,
      n_bins = n_bins,
      split_into_list = TRUE,
      use_names = FALSE,
      shuffle = shuffle
    )
    if (!invert) {
      if (shuffle) {
        res <- lapply(seq_along(res), function(i) unlist(res[-i], use.names = FALSE))
      } else {
        res <- lapply(res, function(z) seq_along(y)[-z])
      }
    }
    if (use_names) {
      names(res) <- .names("Fold", seq_along(res))
      if (m_rep > 1L) {
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

