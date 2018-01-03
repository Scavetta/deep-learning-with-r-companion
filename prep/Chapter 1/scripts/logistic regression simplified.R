# logistic regression simplified:

testData <- data.frame(x = c(rnorm(100),rnorm(100, 6)),
                       y = c(rnorm(100),rnorm(100, 6)),
                       col = rep(c("A", "B"), each = 100))

ggplot(testData, aes(x, y, col = col)) +
  geom_point()

g <- glm(testData$col ~ testData$x + testData$y , family = binomial) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm

# draws a curve based on prediction from logistic regression model
# curve(
  predict(g,data.frame(bodysize = testData$x),type="resp")
  # ,add=TRUE)

points(bodysize,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.



fit = glm(am ~ wt, data=mtcars, family=binomial)
newdat <- data.frame(wt = seq(min(mtcars$wt), max(mtcars$wt),len=100))
newdat$am = predict(fit, newdata=newdat, type="response")
plot(am ~ wt, data = mtcars, col="red4")
lines(am ~ wt, newdat, col="green4", lwd=2)


# get ggplot2 version:
dta <- mtcars
mod <- glm(am ~ wt, data = dta, family=binomial)
## generate prediction frame
pframe <- with(dta,
               expand.grid(wt=seq(min(wt),max(wt),length=32)))
## add predicted values (on response scale) to prediction frame
pframe$num_awards <- predict(mod,newdata=pframe,type="response")

ggplot(mtcars, aes(wt, am)) +
  geom_point(shape = 16, size = 3, alpha = 0.45)  +
  geom_smooth(method = "glm", se = FALSE, col = "red", method.args = list(family = "binomial")) +
  scale_y_continuous("Transmission Type", breaks = c(0,1),
                     labels = c("automatic", "manual")) +
  scale_x_continuous("Weight", expand = c(0,0), limits = c(1, 6)) +
  coord_fixed(1) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.line.y = element_blank())

ggsave("logistic Regression.pdf", height = 2, width = 7.5, units = "in")
