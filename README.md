# splitTools

`splitTools` is a fast, lightweight toolkit for data splitting. Data sets can be split into disjoint groups (e.g. into training/validation/test) or into (repeated) k-folds for subsequent cross-validation. Besides basic splits, the package supports stratified as well as grouped splits.

## Installation
From CRAN:
``` r
install.packages("splitTools")
```

Latest version from github:
``` r
# library(devtools)
install_github("mayer79/splitTools")
```

## Teaser

``` r
library(splitTools)
set.seed(3)

# Train/valid/test indices for iris data stratified by Species
inds <- partition(iris$Species, p = c(train = 0.5, valid = 0.25, test = 0.25))

# Indices for 5-fold cross-validation stratified by Species
inds <- create_folds(iris$Species, k = 5)

# Indices for 3 times repeated 5-fold cross-validation stratified by Species
inds <- create_folds(iris$Species, k = 5, m_repetitions = 3)

```

