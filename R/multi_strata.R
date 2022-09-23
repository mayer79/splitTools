#' Create strata from multiple features
#'
#' This function is a helper function to create strata based on multiple criteria,
#'   i.e. the columns of a data.frame, which can then be used as input variable
#'   to the splitting functions.
#'
#' @importFrom stats quantile
#'
#' @param df A data.frame. All columns of the provided data.frame are used to
#'   compute strata using the defined \code{strategy} (see Details).
#' @param strategy A character. The strategy to compute the strata (see Details).
#' @param k An integer to define the number of groups to categorize
#'   numeric variables when perform splitting on multiple criteria
#'   (Default: \code{NULL}). See Details for further information.
#'
#' @return A vector with \code{m} groups created using interactions between the
#'   features from the provided data.frame that can be passed further on to
#'   the splitting functions.
#'
#' @details The interactions of all columns are calculated to define
#'   \code{m} 'groups', which can be used to perform  the splitting on.
#'   This enables splitting by multiple criteria. While columns of a type
#'   other than \code{numeric} are passed unmodified to \code{interaction()},
#'   a vector of quantiles is computed for numeric variables to group /
#'   categorize the values before passing them further on.
#'   The number of categories to split numeric variables is to be provided
#'   with the argument \code{k}.
#'
#' @seealso [interaction()], [quantile()], [cut()]
#'
#' @export
#' @examples
#' y_multi <- data.frame(
#'   rep(c(letters[1:4]), each = 100),
#'   factor(sample(c(0, 1), 400, replace = TRUE)),
#'   rnorm(400)
#' )
#' multi_strata(y_multi, k = 3)
multi_strata <- function(df, strategy = c("interaction"), k = 3L) {
  strategy  <- match.arg(strategy)
  # Input checks
  stopifnot(
    !is.atomic(df) || ncol(df) > 1,
    is.integer(as.integer(k)) && k > 1L,
    is.data.frame(df)
  )

  message(sprintf("The provided data.frame has %s columns ...", ncol(df)))
  y <- .strata(df = df, k = k)
  message(sprintf("... resulting in %s different groups.", nlevels(y)))
  return(y)
}

.strata <- function(df, k) {
  stopifnot(
    is.integer(as.integer(k)),
    k > 1L
  )
  # +1 required as e.g. cutting a vector with breaks of length 5 results in
  # 4 groups and users define with 'k' the number of categories
  # for numeric variables
  probs <- seq(0, 1, len = (as.integer(k) + 1L))

  strata_var_list <- sapply(
    X = colnames(df),
    FUN = function(cn) {
      col <- df[[cn]]
      if (is.numeric(col)) {
        # according to suggestion by @mayer79 here:
        # https://github.com/mayer79/splitTools/issues/13#issuecomment-1186096681
        breaks <- unique(stats::quantile(col, probs = probs, names = FALSE))
        if (length(breaks) < (length(probs) - 1L)) {
          stop(sprintf(
            paste0("Computation of quantiles for column '%s' results in less ",
                   " groups (=%s) than the number of interaction groups ",
                   "provided (=%s).\n",
                   "Consider to change either the type of column '%s' to ",
                   "'factor' or reduce the number of interaction",
                   " groups (argument 'interact_grps')."
            ), cn, length(breaks), (length(probs) - 1L), cn
          ))
        }
        cut_obj <- cut(col, breaks, include.lowest = TRUE)
        return(cut_obj)
      } else if (is.factor(col) || is.character(col)) {
        return(df[[cn]])
      } else {
        return(NULL)
      }
    },
    simplify = FALSE, # return list that can be passed to `interaction`
    USE.NAMES = TRUE
  )
  strata_var_list <- strata_var_list[!sapply(strata_var_list, is.null)]
  if (length(strata_var_list) > 1L) {
    strata <- interaction(strata_var_list, drop = TRUE, sep = ":")
    return(strata)
  } else {
    stop("Not enough columns in 'df' to compute interactions with.")
  }
}
