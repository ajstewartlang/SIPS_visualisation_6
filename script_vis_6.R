library(ggplot2)
library(ggthemes)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(tools)
library(gganimate)
library(ggimage)
library(gifski)
library(png)

full_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

# Visualisation 6 ####
p <- full_trains %>%
  filter(service == "International" & year == 2017) %>%
  unite(journey, departure_station, arrival_station, sep = " - ") %>% 
  mutate(journey = toTitleCase(tolower(journey))) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.name)) %>%
  mutate(image = "train.png") %>%
  group_by(journey) %>%
  arrange(month) %>% 
  mutate(cum_sum = cumsum((num_late_at_departure))) %>% 
  ggplot(aes(x = journey, y = cum_sum)) +
  geom_image(aes(image = image), size = .15) + 
  guides(colour = FALSE) +
  coord_flip() +
  labs(title = "Running Total of International SNCF Trains Departing Late", 
       subtitle = "{closest_state} 2017", 
       x = NULL, y = "Running Total of Trains Late at Departure") +
  theme_economist() +
  theme(plot.subtitle = element_text(size = 15), 
        axis.title = element_text(size = 13), axis.text = element_text(size = 13)) +
  transition_states(states = month)  

animate(p, height = 500, width = 1000)
