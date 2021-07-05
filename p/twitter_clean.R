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
library(stringr)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

# data import

t_2015 = fread("tweet_df_2015_1_12.csv", stringsAsFactors = F)
t_2016 = fread("tweet_df_2016_1_12.csv", stringsAsFactors = F)
t_2017 = fread("tweet_df_2017_1_12.csv", stringsAsFactors = F)
t_2018 = fread("tweet_df_2018_1_12.csv", stringsAsFactors = F)

t_2014_1_12 = fread("tweet_df_2014_1_12.csv", stringsAsFactors = F)
t_2014_6_12 = fread("tweet_df_2014_6_12.csv", stringsAsFactors = F)
t_2014_1 = fread("tweet_df_2014_1.csv", stringsAsFactors = F)
t_2014_2 = fread("tweet_df_2014_2.csv", stringsAsFactors = F)
t_2014_3 = fread("tweet_df_2014_3.csv", stringsAsFactors = F)
t_2014_4 = fread("tweet_df_2014_4.csv", stringsAsFactors = F)
t_2014_5 = fread("tweet_df_2014_5.csv", stringsAsFactors = F)

tweets = t_2014_1_12 %>%
  rbind(t_2015, fill = TRUE) %>%
  rbind(t_2016, fill=TRUE) %>% # copyright and country witheld columns appear
  rbind(t_2017, fill=TRUE) %>%
  rbind(t_2018, fill = TRUE)

# sending place id's to get approximate latitude longitudes
place_ids = unique(tweets$geo.place_id)
write.table(place_ids, "place_ids.csv", row.names = F, col.names = F)

unique_ids = unique(tweets$id)

tweets_undup = tweets %>% filter(id %in% unique_ids)

## Cleaning ----

tweets_undup = tweets_undup %>%
  mutate(created_at = str_replace(created_at, "T", " "),
         created_at = str_replace(created_at, ".000Z",""),
         datetime = ymd_hms(created_at),
         date = as.Date(datetime)) %>%
  select(-c(created_at))

fwrite(tweets_undup, "undup_tweets.csv", row.names = F)

