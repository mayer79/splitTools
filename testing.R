lapply(list.files("R", full.names = TRUE), source)
# library(splitTools)
library(caret)

#===========================
# PARTITION
#===========================

y <- rep(LETTERS[1:10], each = 10)
# y <- c(y, NA, NA)
# y <- factor(y)

(out <- partition(y, p = c(0.6, 0.2, 0.2)))
length(unique(unlist(out)))
table(y[out$`1`])

p <- c(0.6, 0.2, 0.2)
# p <- c(tr=0.6, v=0.2, te=0.2)
(out <- partition(y, p))
(out <- partition(y, p, type = "grouped"))
(out <- partition(y, p, type = "basic"))

(out <- partition(y, p, split_into_list = F))
(out <- partition(y, p, type = "grouped", split_into_list = F))
(out <- partition(y, p, type = "basic", split_into_list = F))
(out <- partition(y, p, type = "blocked", split_into_list = F))

(out <- partition(y, p, split_into_list = F, use_names = F))
(out <- partition(y, p, type = "grouped", split_into_list = F, use_names = F))
(out <- partition(y, p, type = "basic", split_into_list = F, use_names = F))
(out <- partition(y, p, type = "blocked", split_into_list = F, use_names = F))


(out <- partition(y, p = p))
length(unique(unlist(out)))

(out <- partition(y, p = p, type = "basic"))
length(unique(unlist(out)))
table(y[out$tr])

(out <- partition(y, p = c(tr=0.8, v=0.2), type = "grouped"))
length(unique(unlist(out)))
table(y[out$v])

y <- 10000:1
# y <- c(y, NA, NA)

str(out <- partition(y, p = c(0.6, 0.2, 0.2)))
lapply(out, function(z) mean(y[z], na.rm = TRUE))

out <- partition(y, p = c(0.6, 0.2, 0.2), type = "basic")
lapply(out, function(z) mean(y[z], na.rm = TRUE))

out <- partition(y, p = c(0.6, 0.2, 0.2), type = "grouped")
lapply(out, function(z) mean(y[z], na.rm = TRUE))


#===========================
# FOLDS
#===========================

y <- rep(LETTERS[1:10], each = 10)
# y <- c(NA, NA, y)
# y <- factor(y)
(out <- create_folds(y, k = 3))
sum(sapply(out, length))
(out <- create_folds(y, k = 3, type = "grouped"))
sum(sapply(out, length))
(out <- create_folds(y, k = 3, type = "basic"))
sum(sapply(out, length))
(out <- create_folds(y, k = 3, type = "blocked"))
sum(sapply(out, length))

out <- create_folds(y, k = 3, invert = T)
sum(sapply(out, length))
out <- create_folds(y, k = 3, type = "grouped", invert = T)
sum(sapply(out, length))
out <- create_folds(y, k = 3, type = "basic", invert = T)
sum(sapply(out, length))
out <- create_folds(y, k = 3, type = "blocked", invert = T)
sum(sapply(out, length))

(out <- create_folds(y, k = 3, use_names = F))

y <- sample(LETTERS[1:10], 1e6, T)
# y <- c(NA, NA, y)
# y <- as.factor(y)
system.time(out <- create_folds(y, k = 4))
sd(sapply(out, FUN = function(z) mean(y[z] == "B", na.rm = T)))
system.time(out <- create_folds(y, k = 4, type = "basic"))
sd(sapply(out, FUN = function(z) mean(y[z] == "B", na.rm = T)))
system.time(out <- caret::createFolds(y, k = 10))
sd(sapply(out, FUN = function(z) mean(y[z] == "B", na.rm = T)))

y <- rexp(1e6)
# y <- c(NA, NA, y)

sum(sapply(create_folds(y, k = 10, invert = T), length))
sum(sapply(create_folds(y, k = 10, invert = T, type = "basic"), length))
sum(sapply(create_folds(y, k = 10, invert = T, type = "grouped"), length))
sum(sapply(create_folds(y, k = 10, invert = T, type = "blocked"), length))


system.time(out <- create_folds(y, k = 10))
sd(sapply(out, function(z) mean(y[z], na.rm = T)))
system.time(out <- create_folds(y, k = 10, type = "basic"))
sd(sapply(out, function(z) mean(y[z], na.rm = T)))
system.time(out <- caret::createFolds(y, k = 10))
sd(sapply(out, function(z) mean(y[z], na.rm = T)))



y <- sample(LETTERS[1:2], size = 1e6, T, prob = c(0.9, 0.1))
# y <- factor(y)

system.time(out <- create_folds(y, k = 10))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
system.time(out <- create_folds(y, k = 4, type = "basic"))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))
system.time(out <- caret::createFolds(y, k = 10))
sd(sapply(out, FUN = function(z) mean(y[z] == "B")))

system.time(out <- partition(y, p = c(train = 0.8, valid = 0.2), type = "basic"))


library(microbenchmark)
library(profr)

y <- rexp(1e7)
system.time(out <- create_folds(y, k = 5, type = "basic"))
lapply(out, length)
system.time(out <- create_folds(y, k = 5, approx = TRUE, type = "basic"))
lapply(out, length)

y <- rexp(1e7)
system.time(out <- partition(y, p = c(train = 0.7, test = 0.3), type = "basic"))
lapply(out, length)
system.time(out <- partition(y, p = c(train = 0.7, test = 0.3), type = "basic", approx = TRUE))
lapply(out, length)

# CREATE_TIMEFOLDS
y <- runif(100)
# y <- c(NA, NA, y)
# y <- factor(y)
(out <- create_timefolds(y, k = 3))


library(Rcpp)
sourceCpp("src/C_fast_shuffle.cpp")
