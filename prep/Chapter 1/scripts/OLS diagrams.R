library(ggplot2)

mtcars$cyl <- as.factor(mtcars$cyl)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(rect = element_blank())

ggsave("0_mtcars_points.pdf", height = 9, width = 7.5, units = "in")


ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = F) +
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(rect = element_blank())


ggsave("1_mtcars_LM.pdf", height = 9, width = 7.5, units = "in")

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_smooth(method = "loess", col = "red", se = F) +
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(rect = element_blank())


ggsave("2_mtcars_LOESS.pdf", height = 9, width = 7.5, units = "in")

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), col = "red", se = F)+
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(rect = element_blank())


ggsave("3_mtcars_LMpoly.pdf", height = 9, width = 7.5, units = "in")

# RColorBrewer::brewer.pal(9, "Blues")

ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  scale_color_manual(values = c("#9ECAE1", "#4292C6", "#08519C")) +
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.9),
        rect = element_blank())

ggsave("4_mtcars_groupwise.pdf", height = 9, width = 7.5, units = "in")


ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point() +
  stat_smooth(method = "lm", se = F, fullrange = TRUE) +
  scale_color_manual(values = c("#9ECAE1", "#4292C6", "#08519C")) +
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.9),
        rect = element_blank())

ggsave("5_mtcars_groupwise_full.pdf", height = 9, width = 7.5, units = "in")


myMod <- lm(mpg ~ wt, data = mtcars)

myMod$fitted.values[c(1,3,6)]
myMod$residuals[c(1,3,6)]
mtcars$wt[c(1,3,6)]
mtcars$mpg[c(1,3,6)]

predict(object = myMod, newdata = mtcars$wt)

coef(myMod)[1]
coef(myMod)[2] * -1


selection <- 1:nrow(mtcars)

asp <- (35-10)/(6-1)

asp <- diff(range(mtcars$mpg))/diff(range(mtcars$wt))

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = F) +
  # annotate(geom = "rect", xmin = 1, ymin = 12, xmax = 2, ymax = 14) +
  annotate(geom = "segment",
           x = mtcars$wt[selection],
           y = myMod$fitted.values[selection],
           xend = mtcars$wt[selection],
           yend = mtcars$mpg[selection]) +
  scale_x_continuous("Weight", expand = c(0,0)) +
  scale_y_continuous("mpg", expand = c(0,0)) +
  coord_cartesian(xlim = c(1,6), ylim = c(8,35)) +
  theme_classic() +
  theme(rect = element_blank())


ggsave("6_mtcars_LM_least squares.pdf", height = 9, width = 7.5, units = "in")
%>%
