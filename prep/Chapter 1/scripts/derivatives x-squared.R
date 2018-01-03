# The derivative of x-squared is 2x

rm(list = ls())

DarkRed <- "#C22227"
DarkOrange <- "#F08321"

# Just a single derivative:

x = seq(-6,6,length.out = 800)
y = x^2

DF <- data.frame(x = x,
                 y = y)

# Fit a smoothing spline:
spl <- smooth.spline(DF$y ~ DF$x)
# lines(spl, col=2)

newx <- 2.5
pred0 <- predict(spl, x=newx, deriv=0)
pred1 <- predict(spl, x=newx, deriv=1)

yint <- pred0$y - (pred1$y*newx)
xint <- -yint/pred1$y
xint

DF.tan <- data.frame(x = x,
                     y = yint + pred1$y*x)

# DF.tan <- DF.tan[DF.tan$x > 1.5 & DF.tan$x < 3.5,]

ggplot(DF, aes(x, y)) +
  geom_line() +
  annotate(geom = "point", x = newx, y = newx^2, col = DarkRed, size = 3) +
  # annotate(geom = "text", x = -4.5, y = (-4.5)^2 + 0.4, label = "f(x)", hjust = 0, col = "black", size = 4) +
  annotate(geom = "text", x = newx + 0.4, y = newx^2 - 0.4, label = "Local linear approx.\nof f(x) with slope a\nf'(x) = 2x", hjust = 0, vjust = 1, col = DarkRed, size = 4) +
  geom_line(data = DF.tan, col = DarkRed) +
  scale_x_continuous("x", expand = c(0,0), limits = c(-8,8), breaks = seq(-8,8,2)) +
  scale_y_continuous(expression(paste("f(x) = ",x^{2})), expand = c(0,0), limits = c(0,20)) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.line = element_blank())

ggsave("./prep/Chapter 1/figures/derivatives/calc5 - derivative_full_sing_der_infPoints_vert_horiz.pdf", height = 5, width = 5, units = "in")


