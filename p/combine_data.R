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
  mutate(chng_tweets = num_tweets/lag(num_tweets, 3),
         chng_tweets = chng_tweets-1) %>%
  filter(year == 2017 | year == 2014)

graph2 = merged %>% 
  group_by(treat, year) %>%
  summarise(num_tweets_all = sum(num_tweets, na.rm = T)) %>%
  filter(year <= 2017 & year >= 2014)

graph3 = merged %>% 
  group_by(year, state, treat) %>%
  summarise(num_tweets_state = sum(num_tweets, na.rm = T)) %>%
  filter(year <= 2017 & year >= 2014)

graph4 = graph3 %>%
  ungroup() %>%
  group_by(year, treat) %>%
  summarise(num_tweets_all = sum(num_tweets_state, na.rm = T))

graph5 = merged %>%
  select(year, state, yoy_change, min_wage, treat) %>%
  filter(year <= 2017 & year >= 2014) %>%
  group_by(year, treat) %>%
  summarise(min_wage = mean(min_wage, na.rm = F))

png(filename="../v/inequality_corr_matrix.png", width=2400, height=1500, res = 300)

ggplot(graph, aes(x = three_year_change, y = chng_tweets)) +
  geom_point(aes(color = factor(treat))) + 
  theme_minimal() +
  scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
  labs(x="Change in Minimum Wage", y="Change in Number of Hate Speech Tweets (%)",
       title = "Inequality and Hate Speech")

dev.off()

png(filename="../v/tweet_trends.png", width=2400, height=1500, res = 300)

ggplot(graph2, aes(x = year, y = num_tweets_all)) +
  geom_line(aes(color = factor(treat))) + 
  theme_minimal() +
  scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
  labs(x="Year", y="Number of Tweets Recorded",
       title = "Change in Hate Speech Tweets Over Time")

dev.off()


ggplot(graph3, aes(x = year, y = num_tweets_state)) +
  geom_point() + 
  theme_minimal() +
  scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
  labs(x="Year", y="Number of Tweets Recorded",
       title = "Change in Hate Speech Tweets Over Time")

png(filename="../v/tweet_trends_bar_split.png", width=2400, height=1500, res = 300)

ggplot(graph4, aes(x = year, y = num_tweets_all, fill = factor(treat))) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
  labs(x="Year", y="Number of Tweets Recorded",
       title = "Change in Hate Speech Tweets Over Time")

dev.off()

png(filename="../v/min_wage_trend.png", width=2400, height=1500, res = 300)

ggplot(graph5, aes(x = year, y = min_wage)) +
  geom_line(aes(color = factor(treat))) + 
  theme_minimal() +
  scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
  labs(x="Year", y="Simple Average of Minimum Wage",
       title = "Change in Minimum Wage Over Time")

dev.off()
