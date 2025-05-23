% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_folds.R
\name{create_folds}
\alias{create_folds}
\title{Create Folds}
\usage{
create_folds(
  y,
  k = 5L,
  type = c("stratified", "basic", "grouped", "blocked"),
  n_bins = 10L,
  m_rep = 1L,
  use_names = TRUE,
  invert = FALSE,
  shuffle = FALSE,
  seed = NULL,
  fold_info = FALSE
)
}
\arguments{
\item{y}{Either the variable used for "stratification" or "grouped" splits.
For other types of splits, any vector of the same length as the data
intended to split.}

\item{k}{Number of folds.}

\item{type}{Split type. One of "stratified" (default), "basic", "grouped", "blocked".}

\item{n_bins}{Approximate numbers of bins for numeric \code{y}
(only for \code{type = "stratified"}).}

\item{m_rep}{How many times should the data be split into k folds?
Default is 1, i.e., no repetitions.}

\item{use_names}{Should folds be named? Default is \code{TRUE}.}

\item{invert}{Set to \code{TRUE} in order to receive out-of-sample indices.
Default is \code{FALSE}, i.e., in-sample indices are returned.}

\item{shuffle}{Should row indices be randomly shuffled within folds?
Default is \code{FALSE}.}

\item{seed}{Integer random seed.}

\item{fold_info}{Set to \code{TRUE} to add attributes "fold" and "repeat"
with fold/repetition indices to the output list. Default is \code{FALSE}.}
}
\value{
If \code{invert = FALSE} (the default), a list with in-sample row indices.
If \code{invert = TRUE}, a list with out-of-sample indices.
}
\description{
This function provides a list of row indices used for k-fold cross-validation
(basic, stratified, grouped, or blocked). Repeated fold creation is supported as well.
By default, in-sample indices are returned.
}
\details{
By default, the function uses stratified splitting. This will balance the folds
regarding the distribution of the input vector \code{y}.
(Numeric input is first binned into \code{n_bins} quantile groups.)
If \code{type = "grouped"}, groups specified by \code{y} are kept together
when splitting. This is relevant for clustered or panel data.
In contrast to basic splitting, \code{type = "blocked"} does not sample
indices at random, but rather keeps them in sequential groups.
}
\examples{
y <- rep(c(letters[1:4]), each = 5)
create_folds(y)
create_folds(y, k = 2)
create_folds(y, k = 2, m_rep = 2)
create_folds(y, k = 3, type = "blocked")
}
\seealso{
\code{\link[=partition]{partition()}}, \code{\link[=create_timefolds]{create_timefolds()}}
}
