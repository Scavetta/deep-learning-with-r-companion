# Some matrix algebra:

x <- 1:4
(z <- x %*% x)    # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y %*% z
y %*% x
x %*% z


a <- matrix(1:4, byrow = T, nrow = 2)
b <- matrix(c(7,8,11,12), byrow = F, nrow = 2)

a + b


c <- matrix(1:8, byrow = T, nrow = 2)

b + c
sweep(c, 2, b, `+`)

library(keras)
