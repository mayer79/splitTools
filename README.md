# splitTools <a href='https://github.com/mayer79/splitTools'><img src='man/figures/logo.png' align="right" height="138.5" /></a>


[![CRAN version](http://www.r-pkg.org/badges/version/splitTools)](https://cran.r-project.org/package=splitTools) [![](https://cranlogs.r-pkg.org/badges/splitTools)](https://cran.r-project.org/package=splitTools) [![](https://cranlogs.r-pkg.org/badges/grand-total/splitTools?color=orange)](https://cran.r-project.org/package=splitTools)

`splitTools` is a toolkit for fast data splitting. It does not have any dependencies. 

Its two main functions `partition` and `create_folds` support

- data partitioning (e.g. into training, validation and test),

- creating folds for cross-validation,

- creating *repeated* folds for cross-validation,

- stratified splitting (e.g. for stratified cross-validation), 

- grouped splitting (e.g. for group-k-fold cross-validation) as well as

- blocked splitting (if the sequential order of the data should be retained).

The function `create_timefolds` does time-series splitting where the out-of-sample data follows the (extending or moving) in-sample data.

The result of `create_folds` can be directly passed to the `folds` argument in cross-validation functions of XGBoost or LightGBM.

## Installation

From CRAN:
``` r
install.packages("splitTools")
```

Latest version from github:
``` r
library(devtools)
install_github("mayer79/splitTools", subdir = "release/splitTools")
```

## Teaser

``` r
library(splitTools)

p <- c(train = 0.5, valid = 0.25, test = 0.25)

# Train/valid/test indices for iris data stratified by Species
str(inds <- partition(iris$Species, p, seed = 1))

# List of 3
#  $ train: int [1:73] 1 3 5 7 8 10 12 13 14 15 ...
#  $ valid: int [1:38] 4 9 19 21 27 28 29 30 32 35 ...
#  $ test : int [1:39] 2 6 11 16 18 22 26 37 38 40 ...

# Same, but different output interface
head(inds <- partition(iris$Species, p, split_into_list = FALSE, seed = 1))

# [1] train test  train valid train test 
# Levels: train valid test

# Indices for 5-fold cross-validation stratified by Species
str(inds <- create_folds(iris$Species, k = 5, seed = 1))

# List of 5
#  $ Fold1: int [1:120] 2 4 5 6 7 8 9 10 11 15 ...
#  $ Fold2: int [1:120] 1 2 3 4 5 6 9 10 11 12 ...
#  $ Fold3: int [1:120] 1 2 3 4 6 7 8 9 11 12 ...
#  $ Fold4: int [1:120] 1 3 5 6 7 8 10 11 12 13 ...
#  $ Fold5: int [1:120] 1 2 3 4 5 7 8 9 10 12 ...

# Indices for 3 times repeated 5-fold cross-validation stratified by Species
str(inds <- create_folds(iris$Species, k = 5, m_rep = 3, seed = 1))

# List of 15
#  $ Fold1.Rep1: int [1:120] 2 4 5 6 7 8 9 10 11 15 ...
#  $ Fold2.Rep1: int [1:120] 1 2 3 4 5 6 9 10 11 12 ...
#  $ Fold3.Rep1: int [1:120] 1 2 3 4 6 7 8 9 11 12 ...
#  $ Fold4.Rep1: int [1:120] 1 3 5 6 7 8 10 11 12 13 ...
#  $ Fold5.Rep1: int [1:120] 1 2 3 4 5 7 8 9 10 12 ...
#  $ Fold1.Rep2: int [1:120] 1 2 3 4 5 6 8 9 11 12 ...
#  $ Fold2.Rep2: int [1:120] 1 3 6 7 8 9 10 12 13 14 ...
# [...]

# Indices for time-series splitting
str(inds <- create_timefolds(1:100, k = 5))

# List of 5
# $ Fold1:List of 2
#  ..$ insample : int [1:17] 1 2 3 4 5 6 7 8 9 10 ...
#  ..$ outsample: int [1:17] 18 19 20 21 22 23 24 25 26 27 ...
# $ Fold2:List of 2
#  ..$ insample : int [1:34] 1 2 3 4 5 6 7 8 9 10 ...
#  ..$ outsample: int [1:17] 35 36 37 38 39 40 41 42 43 44 ...
# $ Fold3:List of 2
# [...]
```

