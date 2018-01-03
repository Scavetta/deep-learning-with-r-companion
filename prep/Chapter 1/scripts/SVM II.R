# Kernlab Support Vector Machines on mtcars

library(kernlab)
library(ggplot2)
set.seed(6) #reproducibility
x = rbind(matrix(rnorm(120), , 2), matrix(rnorm(120, mean = 3), , 2)) #vector with 120 rows and 2 columns
y <- rep(c(-1,1), each = 60)



testData <- iris[iris$Species %in% c("setosa", "virginica"),]
testData$Species <- as.character(testData$Species)
d = data.frame(x1 = testData$Sepal.Length, x2 = testData$Sepal.Width, y = testData$Species)

ggplot(d, aes(x1, x2, col = y)) +
  geom_point(shape = 16, size = 3, alpha = 0.45)  +
  labs(x = "Sepal Length", y = "Sepal Width") +
  scale_colour_brewer("Species", palette = "Set1") +
  theme_classic() +
  theme(rect = element_blank(),
        legend.position = c(0.9, 0.9))

ggsave("iris_svm_0.pdf", height = 7, width = 7, units = "in")

svp = ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 1, kernel = "vanilladot")
pdf("svm.pdf", bg = "transparent")
# plot(svp, data = d, xlim = c(2, 4.5), ylim = c(4, 8))
plot(svp, data = d)
str(svp)
dev.off()
sum(d$y == predict(svp, d))


# svp = ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 1, kernel = "rbfdot")
# sum(d$y == predict(svp, d))
# plot(svp, data = d)
#
# svp = ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 10, kernel = "rbfdot")
# sum(d$y == predict(svp, d))
# plot(svp, data = d)

