# DID
# July 2021
# 
# This file takes all the input variables and then runs a regression on them 
# 
# Inputs:
#   Unclear
#   
# Outputs:
#   Unclear

#### Set up environment ----

library(tidyverse)
library(data.table)
library(did)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

### Read in data and assemble final dataset ----

tweets = fread("tweets_final.csv", stringsAsFactors = F)
controls = fread("controls.csv", stringsAsFactors = F)

tweets2 = tweets %>%
  filter(state != "" & !is.na(state) & state != "District of Columbia") %>%
  select(id, state, date) %>%
  rowwise() %>%
  mutate(state_abb = state.abb[which(state.name == state)]) %>%
  ungroup() %>%
  filter(date >= as.Date("2014-10-01"))

tweets2 = tweets2 %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  group_by(year, state_abb) %>%
  summarise(num_tweets = n()) %>%
  mutate(num_tweets = ifelse(year == 2014, num_tweets*4, num_tweets))

final_data = tweets2 %>%
  inner_join(controls, by = c("year", "state_abb" = "state")) %>%
  mutate(state_id = as.numeric(as.factor(state_abb))) %>%
  group_by(state_id) %>%
  mutate(first.treat = ifelse(time-lag(time) == 1 | (year == 2014 & time == 2), year, NA),
         first.treat = ifelse(control == 1, 0, first.treat)) %>%
  fill(first.treat, .direction = "downup")

final_data_ols = final_data %>%
  mutate(time = time - 1)

### using DID package ----

temp = att_gt(yname = "num_tweets",
              tname = "year",
              idname = "state_id",
              gname = "first.treat", 
              data = final_data,
              xformla = ~ num_hate_groups + unemp_rate + pct_male + pct_female + 
                `Below 18` + `18 to 34` + `34 to 56` + `56 to 65` + `65 plus` +
                bach + gpd + hs + med_inc + pct_white + pct_black + pct_AIAN +
                pct_asian + pct_NHPI + pct_two_plus,
              allow_unbalanced_panel = T)

### idk just running OLS

did_reg = lm(num_tweets ~ treat + time + treat*time + num_hate_groups + pct_male +
               unemp_rate + `Below 18` + `18 to 34` + `34 to 56` + `56 to 65` + 
               bach + hs + gpd + med_inc,
             data = final_data_ols)

summary(did_reg)

did_reg2 = lm(num_tweets ~ treat + time + treat*time + num_hate_groups + 
               unemp_rate + `Below 18` + `18 to 34` + `34 to 56` + `56 to 65` + 
               bach + hs + gpd + med_inc,
             data = final_data_ols)

summary(did_reg2)

did_reg3 = lm(num_tweets ~ treat + time + treat*time + num_hate_groups + 
                `Below 18` + `18 to 34` + `34 to 56` + `56 to 65` + 
                bach + hs + gpd + med_inc,
              data = final_data_ols)

summary(did_reg3)


did_reg_no_ctrl = lm(num_tweets ~ treat + time + treat*time,
              data = final_data_ols)

summary(did_reg_no_ctrl)
