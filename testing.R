lapply(list.files("R", full.names = TRUE), source)
# library(splitTools)
library(caret)

#===========================
# PARTITION
#===========================

y <- rep(LETTERS[1:10], each = 10)
# y <- factor(y)

(out <- partition(y, p = c(0.6, 0.2, 0.2)))
stopifnot(length(unique(unlist(out))) == 100)
table(y[out$`1`])

p <- c(0.6, 0.2, 0.2)
# p <- c(tr=0.6, v=0.2, te=0.2)
(out <- partition(y, p))
(out <- partition(y, p, type = "grouped"))
(out <- partition(y, p, type = "basic"))

(out <- partition(y, p, split_into_list = F))
(out <- partition(y, p, type = "grouped", split_into_list = F))
(out <- partition(y, p, type = "basic", split_into_list = F))

(out <- partition(y, p, split_into_list = F, use_names = F))
(out <- partition(y, p, type = "grouped", split_into_list = F, use_names = F))
(out <- partition(y, p, type = "basic", split_into_list = F, use_names = F))


(out <- partition(y, p = p))
stopifnot(length(unique(unlist(out))) == 100)

(out <- partition(y, p = p, type = "basic"))
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

y <- rep(LETTERS[1:10], each = 10)
# y <- factor(y)
(out <- create_folds(y, k = 3))
(out <- create_folds(y, k = 3, type = "grouped"))
(out <- create_folds(y, k = 3, type = "basic"))

(out <- create_folds(y, k = 3, invert = T))
(out <- create_folds(y, k = 3, type = "grouped", invert = T))
(out <- create_folds(y, k = 3, type = "basic", invert = T))

(out <- create_folds(y, k = 3, use_names = F))

y <- sample(LETTERS[1:10], 1e6, T)
system.time(out <- create_folds(y, k = 4))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
system.time(out <- create_folds(y, k = 4, type = "basic"))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
system.time(out <- caret::createFolds(y, k = 10))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))

y <- rexp(1e6)

system.time(out <- create_folds(y, k = 10))
sd(sapply(out, function(z) mean(y[z])))
system.time(out <- create_folds(y, k = 10, type = "basic"))
sd(sapply(out, function(z) mean(y[z])))
system.time(out <- caret::createFolds(y, k = 10))
sd(sapply(out, function(z) mean(y[z])))



y <- sample(LETTERS[1:2], size = 1e6, T, prob = c(0.9, 0.1))
# y <- factor(y)

system.time(out <- create_folds(y, k = 10))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
system.time(out <- create_folds(y, k = 4, type = "basic"))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
system.time(out <- caret::createFolds(y, k = 10))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))

system.time(out <- partition(y, p = c(train = 0.8, valid = 0.2), type = "basic"))
out <- profr(partition(y, p = c(train = 0.8, valid = 0.2), type = "basic"))
plot(out)
