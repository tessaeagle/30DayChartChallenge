library(tidyverse)
library(lubridate)

df <- tibble(
  date = seq(
    mdy("04/01/2021"),
    mdy("04/30/2021"),
    "days"
  )
)

#probably can do this above but not sure how
df <- df %>%
  mutate(group =
    case_when(
      date == "2021-04-01" ~ 0,
      date > "2021-04-01" ~ 1
    )
  )

#code from: https://vietle.info/post/calendarheatmap/
df2 <- df %>% 
  mutate(weekday = wday(date, label = T, week_start = 7), 
         month = month(date, label = T),
         date1 = yday(date),
         week = epiweek(date))

df2$group <- as.factor(df2$group)

df2 %>%  
  ggplot(aes(weekday,-week, fill = group)) +
  geom_tile(colour = "white")+
  scale_fill_manual(values = c("#fbca6b", "#a8dadc"))+
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#465a75"),
    panel.background = element_rect(fill = "#465a75"),
    axis.text = element_text(color = "white", size = 13),
    plot.title = element_text(color = "#F0b7a4", hjust = .5, size = 18),
    plot.subtitle = element_text(color = "white", hjust = .5, size = 26, face = "bold", vjust = -1.5)
  )+
  labs(
    title = "#30DayChartChallenge",
    subtitle = "April 2021"
  )
