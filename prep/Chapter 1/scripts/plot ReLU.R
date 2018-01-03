# Plot ReLU

library(ggplot2)

DF <- data.frame(x = c(0, 0),
                 y = c(0, 0),
                 xend = c(-5, 10),
                 yend = c(0, 10))

DarkRed <- "#C22227"

ggplot(DF, aes(x, y)) +
  geom_segment(aes(xend = xend, yend = yend),
               arrow = arrow(length = unit(0.5,"cm"))) +
  labs(y = "relu output ", x = "input") +
  scale_x_continuous(breaks = -5:10, expand = c(0,0.3)) +
  scale_y_continuous(breaks = 0:10, expand = c(0,0.3)) +
  geom_vline(xintercept = 0, col = "grey50") +
  coord_fixed(1) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(colour = "black"))

ggsave("./prep/Chapter 1/figures/relu/relu.pdf", height = 5, width = 7.5, units = "in")
