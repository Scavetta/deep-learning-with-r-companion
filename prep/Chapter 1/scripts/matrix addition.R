# Matrix addition:

myMAT <- data.frame(x = 0.5,
                    y = 1.0,
                    class = "A")

ggplot(myMAT, aes(x, y)) +
  coord_fixed(1, xlim = c(0,1.75), ylim = c(0, 1.5)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,2,0.5)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,2,0.5)) +
  geom_text(aes(label = class), position = position_nudge(-0.1, 0)) +
  geom_point() +
  geom_segment(aes(xend = x, yend = y,
                   x = 0, y = 0),
               arrow = arrow(length = unit(0.5,"cm"))) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_blank())

ggsave("vectors_1.pdf", height = 7, width = 7, units = "in")

myMAT <- data.frame(x = c(0.5,1),
                    y = c(1.0,0.25),
                    class = c("A","B"))

ggplot(myMAT, aes(x, y)) +
  coord_fixed(1, xlim = c(0,1.75), ylim = c(0, 1.5)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,2,0.5)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,2,0.5)) +
  geom_text(aes(label = class), position = position_nudge(-0.1, 0)) +
  geom_point() +
  geom_segment(aes(xend = x, yend = y,
                   x = 0, y = 0),
               arrow = arrow(length = unit(0.5,"cm"))) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_blank())

ggsave("vectors_2.pdf", height = 7, width = 7, units = "in")

myMAT <- data.frame(x = c(0.5,1, 1.5),
                    y = c(1.0,0.25, 1.25),
                    class = c("A","B","A+B"))

ggplot(myMAT[myMAT$class != "A+B",], aes(x, y)) +
  coord_fixed(1, xlim = c(0,1.75), ylim = c(0, 1.5)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,2,0.5)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,2,0.5)) +
  geom_text(aes(label = class), position = position_nudge(-0.1, 0)) +
  geom_segment(data = myMAT[myMAT$class == "A+B",],
               aes(xend = x, yend = y,
                   x = 0.5, y = 1.0),
               arrow = arrow(length = unit(0.5,"cm")),
               col = "grey50") +
  geom_segment(data = myMAT[myMAT$class == "A+B",],
               aes(xend = x, yend = y,
                   x = 1.0, y = 0.25),
               arrow = arrow(length = unit(0.5,"cm")),
               col = "grey50") +
  geom_text(data = myMAT[myMAT$class == "A+B",],
            aes(label = class), position = position_nudge(0.1, 0),
            col = "grey50") +
  geom_point(data = myMAT[myMAT$class == "A+B",], aes(x, y)) +
  geom_point() +
  geom_segment(aes(xend = x, yend = y,
                   x = 0, y = 0),
               arrow = arrow(length = unit(0.5,"cm"))) +
  theme_classic() +
  theme(rect = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_blank())

ggsave("vectors_3.pdf", height = 7, width = 7, units = "in")

