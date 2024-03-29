% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partition.R
\name{partition}
\alias{partition}
\title{Split Data into Partitions}
\usage{
partition(
  y,
  p,
  type = c("stratified", "basic", "grouped", "blocked"),
  n_bins = 10L,
  split_into_list = TRUE,
  use_names = TRUE,
  shuffle = FALSE,
  seed = NULL
)
}
\arguments{
\item{y}{Either the variable used for "stratification" or "grouped" splits.
For other types of splits, any vector of the same length as the data
intended to split.}

\item{p}{A vector with split probabilities per partition, e.g.,
\code{c(train = 0.7, valid = 0.3)}. Names are passed to the output.}

\item{type}{Split type. One of "stratified" (default), "basic", "grouped", "blocked".}

\item{n_bins}{Approximate numbers of bins for numeric \code{y}
(only for \code{type = "stratified"}).}

\item{split_into_list}{Should the resulting partition vector be split into a list?
Default is \code{TRUE}.}

\item{use_names}{Should names of \code{p} be used as partition names?
Default is \code{TRUE}.}

\item{shuffle}{Should row indices be randomly shuffled within partition?
Default is \code{FALSE}. Shuffling is only possible when \code{split_into_list = TRUE}.}

\item{seed}{Integer random seed.}
}
\value{
A list with row indices per partition (if \code{split_into_list = TRUE})
or a vector of partition IDs.
}
\description{
This function provides row indices for data splitting, e.g., to split data
into training, validation, and test. Different types of split strategies are
supported, see Details.
The partition indices are either returned as list with one element per partition
(the default) or as vector of partition IDs.
}
\details{
By default, the function uses stratified splitting. This will balance the partitions
as good as possible regarding the distribution of the input vector \code{y}.
(Numeric input is first binned into \code{n_bins} quantile groups.)
If \code{type = "grouped"}, groups specified by \code{y} are kept together when
splitting. This is relevant for clustered or panel data.
In contrast to basic splitting, \code{type = "blocked"} does not sample indices
at random, but rather keeps them in groups: e.g., the first 80\% of observations form
a training set and the remaining 20\% are used for testing.
}
\examples{
y <- rep(c(letters[1:4]), each = 5)
partition(y, p = c(0.7, 0.3), seed = 1)
partition(y, p = c(0.7, 0.3), split_into_list = FALSE, seed = 1)
p <- c(train = 0.8, valid = 0.1, test = 0.1)
partition(y, p, seed = 1)
partition(y, p, split_into_list = FALSE, seed = 1)
partition(y, p, split_into_list = FALSE, use_names = FALSE, seed = 1)
partition(y, p = c(0.7, 0.3), type = "grouped")
partition(y, p = c(0.7, 0.3), type = "blocked")
}
\seealso{
\code{\link[=create_folds]{create_folds()}}
}
