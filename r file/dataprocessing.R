library(readr) 
library(tidyverse) 



kanye <- get_artist_audio_features('kanye west') 




Kanye_streams <- read_csv("Kanye streams.csv")

# Joined the

streamsAndStats <- Kanye_streams %>%  
  rename(track_name = Track) %>%  
  select(track_name, Streams) %>% 
  inner_join(kanye, by = "track_name") %>%  
  distinct(track_name, .keep_all = TRUE)

streamsf_plot <- Kanye_streams %>% 
  mutate(collabs = str_count(With, ",") + 1) %>%  
  select(Track, Streams, With, collabs) %>% 
  remove_missing(na.rm = TRUE) %>% 
  ggplot(aes(x = collabs, y = Streams, text = paste(
    Track 
  ))) + 
  geom_point(alpha = 0.5, colour = "red") + 
  labs(x = "Number of Features", 
       y = "Number of Streams", 
       title = "Streams as a function of features", 
       subtitle = "Including singles.")
