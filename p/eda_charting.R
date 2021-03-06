# EDA Charting
# July 2021
# 
# this file charts a few graphs from cleaned data. It's called after
# cleaned data from twitter_clean.R
# 
# previous script: 
#   twitter_clean.R
# 
# inputs: 
#   
# outputs: 
#   place_ids.csv

# set up environment
library(tidyverse)
library(lubridate)
library(data.table)
library(scales)
library(stringr)
library(usmap)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

tweets_final = fread("tweets_final.csv", stringsAsFactors = F)
controls = fread("controls.csv", stringsAsFactors = F)

# mild EDA ----

eda_df = tweets_final %>%
  select(id, date, state) %>%
  mutate(dofw = weekdays(date),
         month = lubridate::month(date, label = TRUE))

dofw_tweets = eda_df %>% 
  select(-c(state)) %>%
  group_by(dofw) %>%
  summarise(day_tweets = n()) %>%
  mutate(dofw = as.factor(dofw), 
         dofw = factor(dofw, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

month_tweets = eda_df %>% 
  select(-c(state)) %>%
  group_by(month) %>%
  summarise(monthly_tweets = n()) %>%
  mutate(month = as.factor(month), 
         month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
daily_tweets = eda_df %>%
  select(-c(state)) %>%
  group_by(date) %>%
  summarise(daily_tweets = n())

state_level_df = eda_df %>%
  group_by(state) %>%
  summarise(state_level_tweets = n()) %>%
  filter(state!="")

state_groups = controls %>%
  select(state, treat) %>%
  unique() %>%
  mutate(treat = ifelse(treat == 1, "Treatment", "Control"),
         treat = as.factor(treat))

## plotting ----

png(filename="../v/dofw_tweets.png", width=800, height=500)

ggplot(dofw_tweets, aes(x = dofw, y = day_tweets)) +
  geom_bar(stat = 'identity') +
  theme_minimal()

dev.off()

png(filename="../v/monthly_tweets.png", width=800, height=500)

ggplot(month_tweets, aes(x = month, y = monthly_tweets)) +
  geom_bar(stat = 'identity') +
  theme_minimal()

dev.off()

png(filename="../v/daily_time_series_tweets.png", width=2000, height=1000)

ggplot(daily_tweets, aes(x = date, y = daily_tweets)) +
  geom_line() + 
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b %Y")) +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45))

dev.off()

png(filename="../v/geo_state_level_tweets.png", width=2000, height=1000)

plot_usmap(data = state_level_df, 
           values = "state_level_tweets",
           regions = c("state"),
           color = "gray0") +
  scale_fill_continuous(low = "lavender", high = "darkblue",
                        name = "No. Tweets", label = scales::comma)+
  labs(Title = "State-Level Tweets in Dataset")

dev.off()

png(filename="../v/state_level_tweets.png", width=2000, height=1000)

sorted = state_level_df[order(state_level_df$state_level_tweets),]

ggplot(sorted,
       aes(x = reorder(state, -state_level_tweets), y = state_level_tweets)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45))

dev.off()

png(filename="../v/state_groupings.png", width=1000, height=500)

plot_usmap(data = state_groups, 
           values = "treat",
           regions = c("state"), 
           color = "grey") +
  scale_fill_brewer(type = 'qual', palette = "Pastel2", name = "Group Assignment") +
  # scale_color_manual(values = c(`0` = "white", `1` = "blue"), na.value = "white") +
  theme(legend.position = "right", 
        legend.text = element_text("Group Assignment"))

dev.off()
