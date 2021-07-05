# CLEANING TWITTER PULL
# June 2021
# 
# this file cleans twitter data pulled from the Academic Research API.
# 
# previous script: 
#   twitter_pull.py
# 
# inputs: 
#   tweet_df_yyyy_m.csv
# outputs: 
#   place_ids.csv


# setting up environment ----

library(tidyverse)
library(lubridate)
library(data.table)
library(scales)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

# data import

t_2015 = fread("tweet_df_2015_1_12.csv", stringsAsFactors = F)
t_2016 = fread("tweet_df_2016_1_12.csv", stringsAsFactors = F)
t_2017 = fread("tweet_df_2017_1_12.csv", stringsAsFactors = F)
t_2018 = fread("tweet_df_2018_1_12.csv", stringsAsFactors = F)

t_2014_6_12 = fread("tweet_df_2014_6_12.csv", stringsAsFactors = F)
t_2014_1 = fread("tweet_df_2014_1.csv", stringsAsFactors = F)
t_2014_2 = fread("tweet_df_2014_2.csv", stringsAsFactors = F)
t_2014_3 = fread("tweet_df_2014_3.csv", stringsAsFactors = F)
t_2014_4 = fread("tweet_df_2014_4.csv", stringsAsFactors = F)
t_2014_5 = fread("tweet_df_2014_5.csv", stringsAsFactors = F)

tweets = t_2014_1 %>%
  rbind(t_2014_2) %>%
  rbind(t_2014_3) %>%
  rbind(t_2014_4) %>%
  rbind(t_2014_5) %>%
  rbind(t_2014_6_12) %>%
  rbind(t_2015) %>%
  rbind(t_2016, fill=TRUE) %>% # copyright and country witheld columns appear
  rbind(t_2017, fill=TRUE) %>%
  rbind(t_2018, fill = TRUE)

place_ids = unique(tweets$geo.place_id)
write.table(place_ids, "place_ids.csv", row.names = F, col.names = F)

unique_ids = unique(tweets$id)

tweets_undup = filter(id %in% unique_ids)

## Cleaning ----

tweets = tweets %>%
  mutate(created_at = str_replace(created_at, "T", " "),
         created_at = str_replace(created_at, ".000Z",""),
         datetime = ymd_hms(created_at),
         date = as.Date(datetime)) %>%
  select(-c(created_at))


# mild EDA ----

eda_df = tweets %>%
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

#graphing 

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
