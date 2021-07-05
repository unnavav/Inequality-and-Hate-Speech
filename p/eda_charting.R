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

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

tweets_final = read.csv("tweets_Final.csv", stringsAsFactors = F)

# mild EDA ----

eda_df = tweets_final %>%
  select(id, date) %>%
  mutate(dofw = weekdays(date),
         month = lubridate::month(date, label = TRUE))

dofw_tweets = eda_df %>% 
  group_by(dofw) %>%
  summarise(day_tweets = n()) %>%
  mutate(dofw = as.factor(dofw), 
         dofw = factor(dofw, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

month_tweets = eda_df %>% 
  group_by(month) %>%
  summarise(monthly_tweets = n()) %>%
  mutate(month = as.factor(month), 
         month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
daily_tweets = eda_df %>%
  group_by(date) %>%
  summarise(daily_tweets = n())

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
