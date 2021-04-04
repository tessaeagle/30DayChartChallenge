library(tidyverse)
library(extrafont)
library(showtext)
library(ggimage)

#data from: https://www.kaggle.com/alex44jzy/harrypotter
#there is a better way to do this but I couldn't get it to work so by hand it is -_-
#I learned about the harry potter package late in this process

img ="https://wallpapercave.com/wp/wp3204926.jpg"


hp1 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/1SorcerersStone.txt")
hp2 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/2ChamberofSecrets.txt")
hp3 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/3ThePrisonerOfAzkaban.txt")
hp4 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/4TheGobletOfFire.txt")
hp5 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/5OrderofthePhoenix.txt")
hp6 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/6TheHalfBloodPrince.txt")
hp7 <- read_file("C:/Users/eagle/Desktop/R/30DayChartChallenge/Code/archive/7DeathlyHollows.txt")

str_count(hp1, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))
str_count(hp2, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))
str_count(hp3, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))
str_count(hp4, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))
str_count(hp5, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))
str_count(hp6, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))
str_count(hp7, c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff"))



hp <- tribble(
  ~book, ~house, ~count,
  1, "Gryffindor", 97,
  2, "Gryffindor", 95,
  3, "Gryffindor", 125,
  4, "Gryffindor", 84,
  5, "Gryffindor", 129,
  6, "Gryffindor", 70,
  7, "Gryffindor", 73,
  1, "Ravenclaw", 11,
  2, "Ravenclaw", 10,
  3, "Ravenclaw", 31,
  4, "Ravenclaw", 21,
  5, "Ravenclaw", 33,
  6, "Ravenclaw", 19,
  7, "Ravenclaw", 48,
  1, "Slytherin", 63,
  2, "Slytherin", 119,
  3, "Slytherin", 65,
  4, "Slytherin", 49,
  5, "Slytherin", 81,
  6, "Slytherin", 56,
  7, "Slytherin",  36,
  1, "Hufflepuff", 19,
  2, "Hufflepuff", 16,
  3, "Hufflepuff", 16,
  4, "Hufflepuff", 34,
  5, "Hufflepuff", 27,
  6, "Hufflepuff", 23,
  7, "Hufflepuff", 11
)

hp <- hp %>%
  mutate(
    group = case_when(
      house == "Gryffindor" ~ 4,
      house == "Slytherin" ~ 3,
      house == "Ravenclaw" ~ 2,
      house == "Hufflepuff" ~ 1
    )
  ) %>%
  mutate(y = book+(group*10))

hp$house <- as.factor(hp$house)
hp$group <- as.factor(hp$group)

showtext_begin()
p<-ggplot(hp)+
  geom_point(aes(x = count, y = y, color = group), shape = 8, size = 1)+
  geom_segment(aes(x = 0, xend = count, y = y, yend = y, color = group), size = .4, show.legend = F)+
  scale_colour_manual(values = c("#FFDB00", "#004dff", "#1D8620", "#AE0001"), labels = c("Hufflepuff", "Ravenclaw", "Slytherin", "Gryffindor"))+
  coord_polar(clip = "off")+
  ylim(0,50)+
  scale_x_continuous(breaks = seq(0, 135, by = 20))+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "white", size = 22),
    plot.title = element_text(hjust = .5, color = "white", family = "HP", size = 80, margin = margin(b = .25, unit = "cm")),
    plot.subtitle = element_textbox_simple(hjust = .5, color = "white", size = 21, lineheight = .4),
    panel.background = element_rect(fill = "#000000"),
    plot.background = element_rect(fill = "#000000"),
    legend.position = "none",
  )+
  guides(color = guide_legend(override.aes = list(size= 3)))+
  labs(
    title = "Harry Potter Houses",
    subtitle = "Count of the number of times each Harry Potter house name occurred in each of the seven books. \nBooks are plotted top to bottom from 1 to 7 and grouped by house. <span style = 'color: #AE0001;'>Gryffindor</span> occurred the most, followed by <span style = 'color: #1D8620;'>Slytherin</span> . <span style = 'color: #004dff;'>Ravenclaw</span> and <span style = 'color: #FFDB00;'>Hufflepuff</span> occurred the least frequently.",
    x = "",
    y = "",
    color = "House"
  )

plot <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#000000", color = NA))

plot <- ggbackground(p, img)

ggsave("Day_4.png", plot, width = 4, height = 5)
