# Kernlab Support Vector Machines
# http://www.machinelearningtutorial.net/2016/12/21/r-kernlab/

library(kernlab)
library(ggplot2)
set.seed(6) #reproducibility
x = rbind(matrix(rnorm(120), , 2), matrix(rnorm(120, mean = 3), , 2)) #vector with 120 rows and 2 columns
y <- rep(c(-1,1), each = 60)

d=data.frame(x=x,y=y)
names(d)<-c("x1", "x2", "y")
qplot(x1, x2, data = d, color = factor(y)) +
  geom_point(shape = 1)  +
  scale_colour_manual(values = c("#0000FF", "#00FF00"), labels = c("1", "-1"))

svp = ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 1, kernel = "vanilladot")
plot(svp, data = d)
sum(d$y == predict(svp, d))

svp = ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 1, kernel = "rbfdot")
sum(d$y == predict(svp, d))
plot(svp, data = d)

svp = ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 10, kernel = "rbfdot")
sum(d$y == predict(svp, d))
plot(svp, data = d)


# Test Data:
testx = rbind(matrix(rnorm(12000), , 2), matrix(rnorm(12000, mean = 3), , 2)) # vector with 24000 rows and 2 columns
sum(d$y == predict(svp, d))
