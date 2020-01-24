#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

if (FALSE) {
  lapply(list.files("R", full.names = TRUE), source)
}

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "splitTools")

create_package(
  pkg,
  fields = list(
    Title = "Tools for Data Splitting",
    Type = "Package",
    Version = "0.2.0",
    Date = Sys.Date(),
    Description = "Fast, lightweight toolkit for data splitting. Data sets can be partitioned into disjoint groups (e.g. into training, validation, and test) or into (repeated) k-folds for subsequent cross-validation. Besides basic splits, the package supports stratified, grouped as well as blocked splitting. See e.g. Hastie et al. (2001) <doi:10.1007/978-0-387-84858-7> for the basic background on data partitioning and cross-validation.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre'))",
    URL = "https://github.com/mayer79/splitTools",
    BugReports = "https://github.com/mayer79/splitTools/issues",
    Depends = "R (>= 3.5.0)",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"))

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back
use_package("stats", "Imports")
# use_rcpp(pkg)

# Set up other files -------------------------------------------------
# use_readme_md()
# use_news_md()
# use_cran_comments()

# Copy readme etc.
file.copy(c("NEWS.md", "README.md", "cran-comments.md", "DESCRIPTION", ".Rbuildignore"), pkg, overwrite = TRUE)

# Copy R scripts and document them
file.copy(list.files("R", full.names = TRUE), file.path(pkg, "R"), overwrite = TRUE)
devtools::document(pkg)

# Check
check(pkg, manual = TRUE)

# Create
build(pkg)
build(pkg, binary = TRUE)

# Install
install(pkg)

# modify .Rbuildignore in build project to ignore the proj file.

check_win_devel(pkg)

check_rhub(pkg)

devtools::release(pkg)

usethis::use_pkgdown()
pkgdown::build_site(pkg)
