# DATA COMBINE
# July 2021
# 
# Combines twitter data with control dataset that's been cleaned and merged together.
# 
# previous scripts:
#   geospatial_match.R
#   controls_clean.R
# 
# Inputs:
#   controls.csv
#   tweets_final.csv
#   
# Outputs:
#   final_dataset.csv
#   

### Set up environment ----

library(data.table)
library(tidyverse)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

### Read in Data ----

controls = fread("controls.csv", stringsAsFactors = F)
tweets = fread("tweets_final.csv", stringsAsFactors = F)

# Combine ----

controls = controls %>% 
  mutate(year = year(observation_date)) %>%
  select(-observation_date)

states_of_interest = controls$state

merged = tweets %>% 
  mutate(year = year(date)) %>% 
  filter(date >= as.Date("2014-10-01")) %>%
  filter((state != "" & !is.na(state)), state != "District of Columbia") %>%
  rowwise() %>%
  mutate(state_abbr = state.abb[which(state.name == state)]) %>%
  ungroup() %>%
  filter(state_abbr %in% states_of_interest) %>%
  select(id, year, state, state_abbr)

merged = merged %>%
  group_by(state, state_abbr, year) %>% 
  summarize(num_tweets = n()) %>%
  mutate(num_tweets = ifelse(year == 2014, num_tweets*4, num_tweets)) %>%
  right_join(controls, by = c("year" = "year", "state_abbr" = "state"))

#### Plotting some elementary stuff ----

graph = merged %>%
  mutate(chng_tweets = num_tweets/lag(num_tweets, 3)) %>%
  filter(year == 2017 | year == 2014)

ggplot(graph, aes(x = three_year_change, y = chng_tweets)) +
  geom_point(aes(color = factor(treat)))
