lapply(list.files("R", full.names = TRUE), source)
library(caret)

#===========================
# PARTITION
#===========================

y <- rep(LETTERS[1:10], each = 10)
# y <- factor(y)

(out <- partition(y, p = c(0.6, 0.2, 0.2)))
stopifnot(length(unique(unlist(out))) == 100)
table(y[out$`1`])

(out <- partition(y, p = c(tr=0.6, v=0.2, te=0.2)))
stopifnot(length(unique(unlist(out))) == 100)

(out <- partition(y, p = c(tr=0.6, v=0.2, te=0.2), type = "basic"))
stopifnot(length(unique(unlist(out))) == 100)
table(y[out$tr])

(out <- partition(y, p = c(tr=0.8, v=0.2), type = "grouped"))
stopifnot(length(unique(unlist(out))) == 100)
table(y[out$v])

y <- 10000:1

out <- partition(y, p = c(0.6, 0.2, 0.2))
lapply(out, function(z) mean(y[z]))

out <- partition(y, p = c(0.6, 0.2, 0.2), type = "basic")
lapply(out, function(z) mean(y[z]))

#===========================
# FOLDS
#===========================

y <- sample(LETTERS[1:10], size = 10000, T)
# y <- factor(y)

out <- create_folds(y, k = 4)
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
out <- create_folds(y, k = 4, type = "basic")
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
out <- caret::createFolds(y, k = 10)
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))

(out <- create_folds(y, p = c(tr=0.8, v=0.2), type = "grouped"))
stopifnot(length(unique(unlist(out))) == 100)
table(y[out$v])

y <- rexp(1e6)

out <- create_folds(y, k = 10)
sd(sapply(out, function(z) mean(y[z])))

out <- create_folds(y, k = 10, type = "basic")
sd(sapply(out, function(z) mean(y[z])))

out <- caret::createFolds(y, k = 10)
sd(sapply(out, function(z) mean(y[z])))



y <- sample(LETTERS[1:2], size = 100000, T, prob = c(0.9, 0.1))
# y <- factor(y)

out <- create_folds(y, k = 10)
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
out <- create_folds(y, k = 4, type = "basic")
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
out <- caret::createFolds(y, k = 10)
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
