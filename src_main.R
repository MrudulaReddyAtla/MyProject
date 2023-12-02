library(ggplot2)
library(dplyr)
library(tidyverse)

taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
write_csv(taylor_album_songs, here::here("data/taylor_album_songs.csv"))

taylor_album_songs %>% 
  group_by(album_name) %>% 
  summarise(energy_mean = mean(energy, na.rm = TRUE), energy_std = sd(energy, na.rm = TRUE))

taylor_album_songs %>% 
  group_by(album_name) %>% 
  summarise(dancebility_mean = mean(danceability, na.rm = TRUE), danceability_std = sd(danceability, na.rm = TRUE))

for (album in unique(taylor_album_songs$album_name)){
  taylor_album_songs %>% 
    as.data.frame %>% 
    filter(album_name == album) %>% 
    ggplot(mapping=aes(x=danceability, y=energy)) + geom_hex()
  
  ggsave(here::here(paste("results/plot_relation_for_", album, ".png")))
}

taylor_album_songs = taylor_album_songs %>% 
  drop_na(danceability) %>%
  drop_na(energy)

taylor_album_songs %>%
  #filter(is.na(danceability,energy)=FALSE) %>%
  group_by(album_name) %>% 
  summarize(correlation = cor(danceability, energy),n= n()) 
