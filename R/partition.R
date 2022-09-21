#' Split Data into Partitions
#'
#' This function provides row indices for data splitting, e.g. into training, validation, and test. Different types of split strategies are supported ("basic", "stratified" (the default), "grouped", or "blocked"), see Details. The partition indices are either returned as a list with one element per partition (the default) or as vector of partition ids.
#'
#' By default, the function uses stratified splitting. This will balance the partitions as good as possible regarding the distribution of the input vector \code{y}. Numeric input is first binned into \code{n_bins} quantile groups. If \code{type = "grouped"}, groups specified by \code{y} are kept together when splitting. This is relevant for clustered or panel data. In contrast to basic splitting, \code{type = "blocked"} does not sample indices at random, but rather keeps them in groups: e.g. the first 80% of observations form a training set and the remaining 20% are used for testing.
#' @importFrom stats ave quantile
#' @param y Either a vector or a data matrix. If a vector, the variable used for "stratification" or "grouped" splits. For other types of splits, any vector of
#'   the same length as the data intended to split.
#'   When providing a data matrix, interactions of all columns are computed on which the splitting is then performed (see Details). A data matrix also
#'   requires to define the argument \code{multi_num_cat}.
#' @param p A vector with split probabilities per partition, e.g. c(train = 0.7, valid = 0.3). Names are passed to the output.
#' @param type Split type. One of "stratified", "basic", "grouped", "blocked". The default is "stratified".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param split_into_list Should the resulting partition vector be split into a list or not? Default is\code{TRUE}.
#' @param use_names Should names of \code{p} be used as partition names? Default is \code{TRUE}.
#' @param shuffle Should row indices be randomly shuffled within partition? Default is \code{FALSE}. Shuffling is only possible when \code{split_into_list = TRUE}.
#' @param multi_num_cat An integer to define the number of groups to categorize numeric variables when perform splitting on multiple criteria (Default: \code{NULL}).
#'   See Details for further information.
#' @param seed Integer random seed.
#' @return A list with row indices per partition (if \code{split_into_list = TRUE}) or a vector of partition ids.
#'
#' @details When providing a data matrix with > 1 columns (e.g. data.frame, matrix, data.table), the interactions of these columns are calculated to define
#'   new 'groups' on which the splitting is then performed. This enables to stratify splitting by multiple criteria. While columns of a type other than \code{numeric}
#'   are passed unmodified to the function \code{interaction}, a vector of quantiles is computed for numeric variables to group / categorize the values before
#'   passing them further on. The number of categories to split numeric variables is to be provided with the argument \code{multi_num_cat}.
#'
#' @seealso [interaction()], [quantile()], [cut()]
#'
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
#'
#' y_multi <- data.frame(
#'   rep(c(letters[1:4]), each = 100),
#'   factor(sample(c(0, 1), 400, replace = TRUE)),
#'   rnorm(400)
#' )
#' partition(y_multi, p = c(0.9, 0.1), multi_num_cat = 3)
partition <- function(y, p,
                      type = c("stratified", "basic", "grouped", "blocked"),
                      n_bins = 10,
                      split_into_list = TRUE,
                      use_names = TRUE,
                      shuffle = FALSE,
                      multi_num_cat = NULL,
                      seed = NULL) {
  # Input checks
  type <- match.arg(type)
  stopifnot(length(p) >= 1L, p > 0,
            is.atomic(y) || ncol(y) > 1,
            ifelse(
              test = !is.atomic(y),
              yes = is.integer(as.integer(multi_num_cat)) && multi_num_cat > 1L,
              no = TRUE
            ),
            ifelse(
              test = !is.atomic(y),
              yes = (n <- nrow(y)) >= 2L,
              no = (n <- length(y)) >= 2L
            )
  )

  # Initializations
  p <- p / sum(p)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # create interactions if data.frame is provided
  if (!is.atomic(y)) {
    if (!is.data.frame(y)) {
      y <- as.data.frame(y)
    }
    message(sprintf("'y' has %s columns.", ncol(y)))
    Sys.sleep(1) # to ensure chronology of messages / warnings
    y <- .strata(y = y, multi_num_cat = multi_num_cat)
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
    out <- ave(integer(n), y, FUN = function(z) .smp_fun(length(z), p))
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
  qu <- quantile(y, seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
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

.strata <- function(y, multi_num_cat) {
  stopifnot(
    is.integer(as.integer(multi_num_cat)),
    multi_num_cat > 1
  )
  # +1 required as e.g. cutting a vector with breaks of length 5 results in
  # 4 groups and users define with 'multi_num_cat' the number of categories
  # for numeric variables
  probs <- seq(0, 1, len = (as.integer(multi_num_cat) + 1L))

  strata_var_list <- sapply(
    X = colnames(y),
    FUN = function(cn) {
      col <- y[[cn]]
      if (is.numeric(col)) {
        # according to suggestion by @mayer79 here:
        # https://github.com/mayer79/splitTools/issues/13#issuecomment-1186096681
        breaks <- unique(stats::quantile(col, probs = probs, names = FALSE))
        if (length(breaks) < length(probs)) {
          stop(sprintf(
            paste0("Computation of quantiles for column '%s' results in less ",
                   " groups (=%s) than the number of interaction groups ",
                   "provided (=%s).\n",
                   "Consider to change either the type of column '%s' to ",
                   "'factor' or reduce the number of interaction",
                   " groups (argument 'interact_grps')."
            ), cn, length(breaks), length(probs), cn
          ))
        }
        cut_obj <- cut(col, breaks, include.lowest = TRUE)
        return(cut_obj)
      } else {
        return(y[[cn]])
      }
    },
    simplify = FALSE, # return list that can be passed to `interaction`
    USE.NAMES = TRUE
  )
  strata <- interaction(strata_var_list, drop = TRUE, sep = ":")
  message(sprintf("Computed %s strata.", nlevels(strata)))
  return(strata)
}
