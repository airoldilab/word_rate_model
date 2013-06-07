Y <- matrix(as.integer(rpois(10*1e4, 10)), 1e4)
d <- (seq(1e4) - 1) %/% 5 + 1
library(Matrix)
dim(Y)
dim(X)
X <- t(sparse.model.matrix(~ factor(d) + 0))
str(X)
system.time(F <- X %*% Y)
str(F)
Y <- matrix(as.integer(rpois(10*1e5, 10)), 1e4)
Y <- matrix(as.integer(rpois(10*1e5, 10)), 1e5)
d <- (seq(1e5) - 1) %/% 5 + 1
summary(d)
X <- t(sparse.model.matrix(~ factor(d) + 0))
system.time(F <- X %*% Y)

