library(tidyverse)
library(ggimage)


tuesdata <- tidytuesdayR::tt_load(2020, week = 15)

tdf_winners <- tuesdata$tdf_winners
stages <- tuesdata$tdf_stages


stages$Date <- as.Date(stages$Date, format="%y-%m-%d")
stages$Date <- format(stages$Date, format="%Y")
stages$Winner_Country <- as.factor(stages$Winner_Country)

df <- stages %>%
  filter(Date > 2015) %>%
  group_by(Date, Winner_Country) %>%
  summarise(count = n()) %>%
  # group_by(Date) %>% 
  # mutate(rank = rank(-count, ties.method = "random")) %>% 
  # ungroup() %>%
  group_by(Winner_Country) %>%
  #keep values that occur in both years
  filter(n() != 1) %>%
  na.omit() 

df$lab <- c(.9,1.2,1.5,7,2,2.3,3,NA,NA,NA,NA,NA,NA,NA)#country labels
df$flagY <- c(.8,1,1.2,7,2,2.2,3,2,.8,5,1,5.2,2.2,1.2)

#rename to full country name
levels(df$Winner_Country) <- list("United Kingdom" = "GBR", "Australia" = "AUS", "Slovakia" = "SVK", "Colombia" = "COL", "France" = "FRA", "Germany" = "GER", "Netherlands" = "NED")

#code from: https://github.com/avrodrigues/Tidy_tuesday/blob/main/2020/week52/big_mac.R
# flags downloaded from: https://www.countryflags.com/
flags <- list.files(pattern = "*.png")
country.flag <- tibble(Winner_Country = c("Australia",
                                "Colombia", 
                                "France", 
                                "Germany", 
                                "Netherlands",
                                "Slovakia",
                                "United Kingdom"),
                       flags = flags)

# join df to flag images
df <- right_join(df, country.flag,  by = "Winner_Country")

ggplot()+
  geom_segment(data = df, aes(x = Date, xend = Date, y = -.5, yend = 8), size = 1.25, color = "black")+
 # geom_point(data = df, aes(x = Date, y = count, color = Winner_Country), size = 2, stroke = 1)+
  geom_text(data = df, (aes(x = 2.1, y = count, label = count)), stat = "unique", size = 5)+#plot text only once
  geom_text(data = df, (aes(x = .8, y = lab, label = Winner_Country)), stat = "unique")+#plot text only once
  geom_line(data = df, aes(x = Date, y = flagY, group = Winner_Country, color = Winner_Country), size = 2)+
  scale_color_manual(values = c("#00843d", "#fdd116", "#ee2436", "#000000", "#ff9a00", "#034da3", "#c9072a"))+
  geom_image(data = df, aes(x = Date, y=flagY, image = flags),size = 0.025, asp = 1.5)+
  geom_curve(aes(x = 2.15, xend = 2.04, y = 1.2, yend = 1), curvature = .2, size = 0.25, colour = "#264653", arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes(x = 2.17, y = 1.1, label = "Sagan DQed (SVK) \nCavendish injured \n(GBR)"), hjust = 0, size = 3.5, colour = "#264653") +
  geom_curve(aes(x = 2.15, xend = 2.04, y = 5.2, yend = 5), curvature = .2, size = 0.25, colour = "#264653", arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes(x = 2.17, y = 5.25, label = "Marcel Kittel (GER)"), hjust = 0, size = 3.5, colour = "#264653") +
  ylim(-.5,8)+
  scale_x_discrete(position = "top")+
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x.top = element_text(size = 12, face = "bold", margin = margin(b = -.25, unit = "cm")),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(l = 3, r = 3, unit = "cm"),
    plot.title = element_text(color = "black", hjust = .5, size = 20, face = "bold", vjust = -.1),
    plot.subtitle = element_text(color = "black", hjust = .5, size = 11),    
    plot.caption = element_text(hjust = 1.25, size = 11)
  )+
  labs(
    title = "TOUR DE FRANCE STAGE WINS",
    subtitle = "A count of the number of stage wins by rider nationality for the 2016 and 2017 Tours de France. \nGreat Britain took home the GC in 2016 and 2017, but only had 1 stage win in 2017, likely due to \nMark Cavendish crashing out. Slovakia's wins decreased due to the questionable disqualification\n of their star sprinter Peter Sagan. These events may have opened the door for sprinter Marcel \nKittel of Germany. Flags that are offset correspond to the same win count for visibility.",
    caption = "Plot: @tessuheagle | Data: Alastair Rushworth"
  )

ggsave("Day_5.png", p, width = 4, height = 3)
