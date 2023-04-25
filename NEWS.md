# splitTools 1.0.0

- Small maintenance update
- Version 1.0.0 indicates maturity

# splitTools 0.3.3

## New Functionality

- Sometimes, one need to stratify on multiple columns. The new function `multi_strata()` provides a vector of stratification groups based on a data frame that can be then passed to `partition()` or `create_folds()`. Each stratification group will contain "similar" data rows, where similarity is either based on a kmeans cluster analysis or forming all combinations of binned columns. Thanks to [kapsner](https://github.com/kapsner) for the idea and the help with the implementation.

## Maintenance

- Set up github actions, thanks to L. Kapsner.

# splitTools 0.3.2

This is documentation and maintenance update only with the following changes:

- Updated documentation to clarify that `create_folds()` creates in-sample indices by default. If out-of-sample indices are to be generated, set `invert = TRUE`.
- Got rid of a CRAN check notes about LazyData.
- Changed to better way of updating/generating the package.

# splitTools 0.3.1

## New Functionality

- `create_folds` and `partition` have received a `shuffle` option to shuffle rows within folds/partitions. The default is FALSE.

# splitTools 0.3.0

## Breaking change for tiny data sets

- `create_folds` and `partition` cannot return empty folds/partitions anymore. This impacts only extremely small data sets.

## Other

- Unit tests have been added.

# splitTools 0.2.1

## New Functionality

- `create_timefolds` now allows also moving windows training data, not just extending windows data.

## Other

Reduced minimally required R version from 3.5 to 3.1.

# splitTools 0.2.0

## New Functionality

- Added `type = "blocked"` to `create_folds` and `partition` to allow for blocked splitting.

- Added function `create_timefolds` for cross-validation of time series data.

## Other

Added vignette

# splitTools 0.1.0

This is the initial CRAN release.
