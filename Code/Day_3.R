library(tidyverse)
library(ggridges)
library(RColorBrewer)


options(scipen = 999)

rt <- read.csv("C:/Users/eagle/Desktop/R//RescueTime/rescuetime_history.csv")

rt <- rt %>% mutate(DateTime = str_replace(DateTime, "\\s", "|")) %>% 
  separate(DateTime, into = c("Date", "Time"), sep = "\\|")


rt$Date1 <- as.Date(rt$Date, format="%m/%d/%Y")
rt$day <- weekdays(as.Date(rt$Date1))
rt$day <- fct_relevel(rt$day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")


df <- rt %>%
  select(Application, TimeSpent, Date1, day) %>%
  filter(str_detect(Application, "zoom")) %>%
  group_by(Date1, day) %>%
  summarise(time = sum(TimeSpent)) %>%
  mutate(
    timeMins = round(time / 60,0)
  ) %>%
  filter(timeMins > 9)

ggplot(df)+
  geom_point(aes(x = Date1, y = timeMins, color = day), size = 2)+
  geom_smooth(aes(x = Date1, y = timeMins), method = lm, color = "#fff870")+
  scale_color_brewer(palette = "Set3")+
  guides(colour = guide_legend(nrow = 1))+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 10),
    legend.title = element_text(color = "white", size = 13),
    panel.grid = element_line(color = "#000000"),
    axis.text = element_text(color = "white", size = 12),
    axis.title.y = element_text(color = "white", size = 14, margin = margin(r = .25, unit = "cm")),
    plot.title = element_text(color = "#2d8cff", size = 20, hjust = .5, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 12, hjust = .5),
    legend.position = "top",
    axis.ticks = element_blank()
  )+
  labs(
    x = "",
    y = "Minutes",
    color = "Day",
    title = "zoom University",
    subtitle ="Time in minutes I spent on Zoom from 12/2019 to 2/2021. The average time spent across \n192 days was 59.7 minutes. Wednesdays and Thursdays tend to be class and meeting heavy."
  )
