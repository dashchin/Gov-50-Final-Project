library(readr) 
library(tidyverse) 

# Tibble of popular Frank Sinatra songs internationally
sinatra <- read_csv("top50country.csv") %>% 
  filter(artist == "Frank Sinatra") %>% 
  group_by(title) %>% 
  summarize(total = n()) 

View(sinatra) 

# Earth, Wind, & Fire active years
ewf <- read_csv("spotify160k-data.csv") %>% 
  filter(artists == "['Earth, Wind & Fire']") %>% 
  mutate(release_year = substring(release_date, 1, 4)) %>%
  group_by(release_year) %>% 
  summarize(total = n()) 

View(ewf)

write_rds(sinatra, path = "sinatra.rds")

write_rds(ewf, path = "ewf.rds")
