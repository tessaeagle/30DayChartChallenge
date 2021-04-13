library(tidyverse)

df <- tibble(
  date = seq(1:12),
  done = c("yes","yes","yes","yes","yes","yes","yes","no","no","no","no","no"),
  y = rep(1,12)
)


p<-ggplot(df, aes(x = date, y = y, fill = done))+
  geom_tile(aes(width=0.5), color = "black")+
  scale_fill_manual(values = c("#f0b99e", "#95f2f5"))+
  annotate("text", x = 1.9, y = 1.55, label = "COMPLETED", color = "#95f2f5", size = 5)+
  annotate("text", x = 10.6, y = 1.55, label = " NOT COMPLETED", color = "#f0b99e", size = 5)+
  annotate("text", x = 5, y = 1.62, label = "Adopted a dog", hjust = 0, size = 4, color = "white") +
  geom_curve(aes(x = 7, xend = 7.7, y = 1.6, yend = 1.5), curvature = -.2, size = .5, color = "white", arrow = arrow(length = unit(0.01, "npc"))) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#101a35"),
    plot.background = element_rect(fill = "#101a35", color = NA),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title = element_text(color = "white", face = "bold", size = 14),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "white", size = 12, margin = margin(t = -.25, b = .5, unit = "cm"), face = "bold"),
    plot.title = element_text(hjust = .5, color = "white", size = 20)
  )+
  labs(
    x = "Day of Challenge",
    y = "",
    title = "#30DayChartChallenge Plots Completed"
  )

ggsave("Day_12.png", p, width = 7, height = 5, dpi = 320)  
