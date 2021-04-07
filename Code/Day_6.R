library(tidyverse)
library(wesanderson)
library(ggpubr)
library(patchwork)

df <- tibble(
  x = 1:40,
  xend = 1:40,
  y = 0,
  yend = rep(c(1, 1.5, 1.75, 2), times = 10),
  group = rep(c(1, 2, 3, 4), times = 10)
)

pal <- wes_palette("Moonrise3", 4, type = "continuous")
pal2 <- wes_palette("GrandBudapest1", 4, type = "continuous")
pal3 <- wes_palette("Darjeeling1", 4, type = "continuous")
pal4 <- wes_palette("GrandBudapest2", 4, type = "continuous")


p<-ggplot()+
  geom_segment(data = df, aes(x = x, xend = xend, y = y, yend = yend, color = group))+
  geom_point(data = df, aes(x = x, y = yend), shape = 8, color = "white", size = 1)+
  coord_polar()+
  ylim(-.5,2)+
  scale_color_gradientn(colors = pal)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.margin=unit(c(0,-4,0,-1), "cm")    
  )


df2 <- tibble(
  x = 0,
  xend = rep(c(1, 2, 3, 4), times = 10),
  y = 1:40, 
  yend = 1:40,
  group = rep(c(1, 2, 3, 4), times = 10)
)


p2<-ggplot()+
  geom_segment(data = df2, aes(x = x, xend = xend, y = y, yend = yend, color = group))+
  coord_polar()+
  scale_color_gradientn(colors = pal2)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.margin=unit(c(0,-1,0,-4), "cm")    
  )

df3 <- tibble(
  x = 0,
  xend = 5,
  y = seq(1,40,by = 2),
  yend = seq(1,40,by = 2),
  group = rep(c(1, 2, 3, 4), times = 5)
)


p3<-ggplot()+
  geom_segment(data = df3, aes(x = x, xend = xend, y = y, yend = yend, color = group))+
  ylim(-10,40)+
  coord_polar()+
  scale_color_gradientn(colors = pal3)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.margin=unit(c(0,-4,0,-1), "cm")    
  )

df4 <- tibble(
  x = 0,
  xend = 1:20,
  y = seq(1,40,by = 2),
  yend = seq(1,40,by = 2),
  group = rep(c(1, 2, 3, 4), times = 5)
)


p4<-ggplot()+
  geom_segment(data = df4, aes(x = x, xend = xend, y = y, yend = yend, color = group))+
  ylim(-10,40)+
  coord_polar()+
  scale_color_gradientn(colors = pal4)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.margin=unit(c(0,-1,0,-4), "cm")
  )

# df5 <- tibble(
#   x = 1:40,
#   xend = 1:40,
#   y = 0, 
#   yend = 1:40,
#   group = rep(c(1, 2, 3, 4), times = 10)
# )
# 
# ggplot()+
#   geom_segment(data = df5, aes(x = x, xend = xend, y = y, yend = yend, color = group))+
#   ylim(-10,40)+
#   coord_polar()+
#   scale_color_viridis()+
#   theme_minimal()+
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     legend.position = "none",
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black")
#   )



plot <- ggarrange(p,p2,p3,p4)


plot1 <- cowplot::ggdraw(plot) + 
  theme(plot.background = element_rect(fill="#000000", color = NA))


ggsave("Day_6.png", plot1, height = 6, width = 8, dpi = 150)

