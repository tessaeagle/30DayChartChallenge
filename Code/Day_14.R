library(tidyverse)
#library(showtext)
library(ggimage)
library(extrafont)

font_import()
loadfonts(device = "win")

db <- list.files(pattern = "*.png")


#font_add_google("Kumar One Outline", "Kumar")


pts <- tibble(
  x = rep(1:18,18),
  y = rep(1:18, each=18),
  group = rep(c(1:9,9:1),18)
)

vseg <- tibble(
  x = c(1:8,11:18),
  xend = c(1:8,11:18),
  y = c(1:8,8:1),
  yend = c(19:12,12:19),
  group = c(1:8,8:1),
)

hseg <- tibble(
  x = c(1:9,9:1),
  xend = c(18:10,10:18),
  y = c(1:18),
  yend = c(1:18),
  group = c(1:9,9:1),
)

p<-ggplot()+
  #geom_segment(x = 1, xend = 1, y = 0, yend = 19, size = 10, color = "#5f998f")+
  geom_segment(data = vseg, aes(x = x, xend = xend, y = y, yend = yend, color = group), size = 9)+
  geom_segment(data = hseg, aes(x = x, xend = xend, y = y, yend = yend, color = group), size = 9)+
  scale_color_gradient(high = "#c7d659", low = "#518a80")+
  geom_point(data = pts, aes(x = x, y = y, fill = group), size = 6, shape = 21, color = "NA")+
  scale_fill_gradient(high = "#34aac1", low = "#3a5276")+
  annotate("text", label = "DAVID BOWIE", x = 10, y = 16, color = "white", size = 10, family = "Bauhaus 93")+
  geom_image(data = df, aes(x = 9.5, y = 10, image = db), size = .5, asp = 1, alpha = .1)+
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.background = element_blank(),
    panel.background = element_blank()
  )+
  labs(
    x = "",
    y = ""
  )

ggsave("Day_14.png", p, width = 5, height = 5)


