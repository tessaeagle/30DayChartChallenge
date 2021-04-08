library(tidyverse)
library(jsonlite)
library(ggridges)
library(wesanderson)
library(ggtext)

pal <- wes_palette("GrandBudapest2", 7, type = "continuous")

d1 <- fromJSON("steps-2020-01-20.json", flatten=TRUE)
d2 <- fromJSON("steps-2020-02-19.json", flatten=TRUE)
d3 <- fromJSON("steps-2020-03-20.json", flatten=TRUE)
d4 <- fromJSON("steps-2020-04-19.json", flatten=TRUE)
d5 <- fromJSON("steps-2020-05-19.json", flatten=TRUE)
d6 <- fromJSON("steps-2020-06-18.json", flatten=TRUE)
d7 <- fromJSON("steps-2020-07-18.json", flatten=TRUE)
d8 <- fromJSON("steps-2020-08-17.json", flatten=TRUE)
d9 <- fromJSON("steps-2020-09-16.json", flatten=TRUE)
d10 <- fromJSON("steps-2020-10-16.json", flatten=TRUE)
d11 <- fromJSON("steps-2020-11-15.json", flatten=TRUE)
d12 <- fromJSON("steps-2020-12-15.json", flatten=TRUE)

merged <- do.call("rbind", list(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12))
merged$value <- as.numeric(merged$value)

new <- do.call( rbind , strsplit( as.character(merged$dateTime ) , " " ) )
df <- cbind( merged , Date = new[,1] , Time = new[,2] )

df$Date1 <- as.Date(df$Date, format="%m/%d/%Y")

df$weekday <- weekdays(as.Date(df$Date1))

agg <- df %>%
  select(value, Date1, weekday) %>%
  group_by(Date1, weekday) %>%
  summarise(total = sum(value)) 


agg$weekday <- factor(agg$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

agg$Date1 <- as.Date(agg$Date1, format="%Y-%m-%d")

agg <- agg %>%
  separate(Date1, sep="-", into = c("year", "month", "day"))

agg$month <- as.numeric(agg$month)

#attempting to do pre and post-pandemic onset but scrapped it
agg <- agg %>%
  mutate(time =
      case_when(
         month < 03 ~ "Pre",
         month > 03 ~ "Post"
     ))%>%
  select(4:6)


p<-ggplot(agg, aes(x = total, y = weekday, fill = weekday, color = weekday)) + 
  geom_density_ridges(alpha = 1)+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, color="#b4d6e0") +
  #geom_density_ridges(aes(point_color = time), jittered_points = TRUE, position = "raincloud", scale = 0.75, color = NA, point_alpha = 1)+
  scale_fill_manual(values = pal)+
  #scale_color_manual(values = pal)+
  scale_discrete_manual("point_color", values = c("#f4b5bd", "#85d4e3")) +
  scale_x_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000))+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#465a75"),
    plot.background = element_rect(fill = "#465a75", color = NA),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_text(margin = margin(b = .5, unit = "cm"), size = 12, color = "white"),
    axis.text.y = element_text(margin = margin(r = -.5, unit = "cm"), size = 13, face = "bold", color =pal[agg$weekday]),
    axis.title = element_text(size = 12, color = "white", face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = .45, color = "#aee5f5"),
    plot.subtitle = element_text(size = 12, color = "#aee5f5", margin = margin(b = .5, unit = "cm")),
    plot.caption = element_text(size = 9.5, color = "#aee5f5", hjust = 1, margin = margin(t = .5, unit = "cm"))
  )+
  labs(
    x = "Total Steps",
    y = "",
    caption = "Plot: @tessuheagle | #30DayChartChallenge Day 7",
    title = "STEP COUNT 2020",
    subtitle = "Distribution of my 2020 step data across days of the week. The date range \nincluded is 1-21-20 to 12-22-20 as recorded by my Fitbit. The median steps \nfor each weekday are indicated by the vertical blue lines and tend to be a bit \nhigher on Saturday and Sunday "
  )

ggsave("Day_7.png", p, width = 7, height = 5, dpi = 300)
