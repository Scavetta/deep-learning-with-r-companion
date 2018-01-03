# Calculating and plotting derivatives:
rm(list = ls())

DarkRed <- "#C22227"
DarkOrange <- "#F08321"

# Just a single derivative:

x = seq(0,6,length.out = 400)
y = sin(x)

DF <- data.frame(x = x,
                 y = y)

# plot(DF, type="l")

spl <- smooth.spline(DF$y ~ DF$x)
# lines(spl, col=2)

newx <- 2.5
pred0 <- predict(spl, x=newx, deriv=0)
pred1 <- predict(spl, x=newx, deriv=1)

yint <- pred0$y - (pred1$y*newx)
xint <- -yint/pred1$y
xint

# plot(DF, type = "l")
# # abline(h=0, col=8)
# lines(spl, col=2) # spline
# points(pred0, col=2, pch=19) # point to predict tangent
# lines(x, yint + pred1$y*x, col=3) # tangent (1st deriv. of spline at newx)
# # points(xint, 0, col=3, pch=19) # x intercept
#

DF.tan <- data.frame(x = x,
                     y = yint + pred1$y*x)

DF.tan <- DF.tan[DF.tan$x > 1.5 & DF.tan$x < 3.5,]

ggplot(DF, aes(x, y)) +
  geom_line() +
  annotate(geom = "point", x = newx, y = sin(newx), col = DarkRed, size = 3) +
  # annotate(geom = "text", x = 0 + 0.2, y = sin(0), label = "f(x)", hjust = 0, col = "black", size = 4) +
  annotate(geom = "text", x = newx + 0.2, y = sin(newx), label = "Local linear approx.\nof f(x) with slope a", hjust = 0, col = DarkRed, size = 4) +
  geom_line(data = DF.tan, col = DarkRed) +
  scale_x_continuous(expand = c(0,0), limits = c(0,6)) +
  scale_y_continuous("f(x)", expand = c(0,0), limits = c(-1.5,1.5), breaks = seq(-1.5,1.5,0.5)) +
  theme_classic() +
  theme(rect = element_blank(),
        # axis.text.x = element_blank(),
        # axis.title = element_blank(),
        axis.line = element_blank())

ggsave("./prep/Chapter 1/figures/derivatives/calc0 - derivative.pdf", height = 5, width = 5, units = "in")

# The whole curve:
dd <- DF

fit <- lm(y ~ poly(x,32,raw=TRUE), dd)
dd$fitted <- fitted(fit)

ggplot(dd, aes(x=x)) +
  geom_line(aes(y = y), colour=DarkRed) +
  geom_line(aes(y = fitted), colour="blue")


deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)

# ggplot(dd, aes(x=x)) +
#   geom_line(aes(y = y), colour=DarkRed) +
#   geom_line(aes(y = fitted), colour="blue")


p <- ggplot(DF, aes(x, y)) +
  geom_line() +
  # annotate(geom = "text", x = 0 + 0.2, y = sin(0), label = "f(x)", hjust = 0, col = "black", size = 4) +
  geom_line(data = dd, aes(y = slope), colour=DarkOrange) +
  annotate(geom = "text", x = 0 , y = 0.9, label = "f'(x)", hjust = 0, col = DarkOrange, size = 4) +
  annotate(geom = "point", x = newx, y = sin(newx), col = DarkRed, size = 3) +
  annotate(geom = "text", x = newx + 0.2, y = sin(newx), label = "Local linear approx.\nof f(x) with slope a", hjust = 0, col = DarkRed, size = 4) +
  geom_line(data = DF.tan, col = DarkRed) +
  scale_x_continuous(expand = c(0,0), limits = c(0,6)) +
  scale_y_continuous("f(x)", expand = c(0,0), limits = c(-1.5,1.5), breaks = seq(-1.5,1.5,0.5)) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.line = element_blank())

p
ggsave("./prep/Chapter 1/figures/derivatives/calc1 - derivative_full.pdf", height = 5, width = 5, units = "in")


p <- p + annotate(geom = "point", x = newx, y = pred1$y, col = DarkOrange, size = 3)
p
ggsave("./prep/Chapter 1/figures/derivatives/calc2 - derivative_full_sing_der.pdf", height = 5, width = 5, units = "in")

# c(DF$x[which.max(DF$y)], DF$x[which.min(DF$y)])

p <- p + annotate(geom = "point",
             x = c(DF$x[which.max(DF$y)], DF$x[which.min(DF$y)]),
             y = c(DF$y[which.max(DF$y)], DF$y[which.min(DF$y)]),
             col = DarkRed, size = 3)
p
ggsave("./prep/Chapter 1/figures/derivatives/calc3 - derivative_full_sing_der_infPoints.pdf", height = 5, width = 5, units = "in")

p <- p + geom_vline(xintercept = c(DF$x[which.max(DF$y)], DF$x[which.min(DF$y)]), col = "grey50")
p
ggsave("./prep/Chapter 1/figures/derivatives/calc4 - derivative_full_sing_der_infPoints_vert.pdf", height = 5, width = 5, units = "in")

p <- p + geom_hline(yintercept = 0, col = "grey50")
p
ggsave("./prep/Chapter 1/figures/derivatives/calc5 - derivative_full_sing_der_infPoints_vert_horiz.pdf", height = 5, width = 5, units = "in")


# Make animation of this process?
