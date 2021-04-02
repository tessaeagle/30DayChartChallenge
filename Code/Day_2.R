library(tidyverse)
library(waffle)
library(extrafont)
library(ggtext)
library(showtext)
library(RColorBrewer)


loadfonts(device = "win")

goodreads <- read.csv("C:/Users/eagle/Desktop/R/goodreads.csv")


sub <- goodreads %>%
  filter(Bookshelves != "to-read") %>%
  select(1:3,8:15,23) 

sub$yearRead = format(as.Date(sub$Date.Read, format="%Y/%m/%d"),"%Y")


yearly <- sub %>% 
  filter(yearRead > 2010) %>%
  group_by(yearRead) %>%
  na.omit() %>%
  summarise(count = n())

yearly$yearRead <- as.factor(yearly$yearRead)

fa_grep("book")

#code from Sharla Gelfand: https://github.com/sharlagelfand/30DayChartChallenge/blob/main/01-part-to-whole/part-to-whole.R
#for year labels, not working when plotting
yearLabels <- tribble(
  ~yearRead, ~x, ~y, ~angle,
  "2017", 0, 1.5, 90,
  "2018", 3, 11, 0,
  "2019", 5, 11, 0,
  "2020", 8, 11, 0,
  "2021", 11, 10, 90
) %>%
  left_join(yearly, by = "yearRead") %>%
  mutate(yearLab = c("2017", "2018", "2019", "2020", "2021"))#attempt to plot as text instead of blank boxes

colors <- RColorBrewer::brewer.pal(5, "Set3")[c(1:5)]


ggplot(yearLabels, aes(label = yearRead, values = count, color = yearRead)) +
  geom_pictogram(n_rows = 5, make_proportional = TRUE, family = "Font Awesome 5 Free Solid") +
  scale_label_pictogram(name = NULL, values = c("book-open", "book-open", "book-open", "book-open", "book-open")) +
  scale_color_manual(name = NULL, values = colors)+
  #geom_text(aes(x = x, y = y, label = yearLab, angle = angle), family = "Times New Roman") + #issue with this plotting as squares?
  coord_equal() +
  theme_enhance_waffle()+
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#465a75"),
    panel.background = element_rect(fill = "#465a75"),
    plot.title = element_text(color = "white", size = 26, hjust = .5, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 12, hjust = .5, margin = margin(b = 1, unit = "cm")),
    plot.caption = element_text(color = "white", size = 12, hjust = 1)
  )+
  labs(
    title = "Yearly Books Read",
    subtitle = "Plot depicting the number of books I read each year from 2017 to 2021, \nas recorded on Goodreads. Each icon represents 3 books.",
    caption = "Plot: @tessuheagle | Data: Personal Goodreads"
  )



ggsave("Day_2.png", p, width = 5, height = 5)

