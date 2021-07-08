# CLEANING CONTROL DATA
# July 2021
# 
# This file takes in control variables and cleans them up so that I can use 
# 'em for analysis later. 
# 
# Inputs:
#   Unclear
#   
# Outputs:
#   Unclear


# Setting up environment ----

library(tidyverse)
library(data.table)
library(readxl)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

# Minimum wage ---- 

## read in data

min_wage_pt1 = read_excel("state_min_wage.xls", sheet = 'Annual_1')
min_wage_pt2 = read_excel("state_min_wage.xls", sheet = 'Annual_2')

## clean 

### clean up names
min_wage = cbind(min_wage_pt1, min_wage_pt2) %>% select("observation_date",contains("20210104"))
colnames(min_wage) = gsub("STTMINWG","", colnames(min_wage))
colnames(min_wage) = gsub("_20210104","", colnames(min_wage))

### clean up dates
min_wage = min_wage %>% 
  mutate(observation_date = gsub(" UTC", "", observation_date),
         observation_date = as.Date(observation_date)) %>%
  filter(observation_date >= as.Date("2009-01-01"))

#### wide to long

min_wage_l = min_wage %>% 
  gather(state,min_wage,AK:WY)

## mark if states didn't change from 2009 to 2016
min_wage_l = min_wage_l %>%
  group_by(state) %>%
  mutate(yoy_change = min_wage/lag(min_wage),
         ten_year_change = min_wage/lag(min_wage, 10),
         three_year_change = min_wage/lag(min_wage, 3)) %>%
  mutate(treat = ifelse((observation_date >= as.Date("2014-01-01") & 
                        observation_date <= as.Date("2016-01-01") &
                        yoy_change >= 1.05), 
                        1, 0), 
         control = ifelse((ten_year_change == 1 &
                             observation_date == as.Date("2020-01-01")), 1, 0)) %>%
  mutate(treat = max(treat, na.rm = T),
         control = max(control, na.rm = T)) %>%
  filter(treat == 1 | control == 1) %>%
  select(-control)

#### Writing Out ----

controls = min_wage_l

write.csv(controls, "controls.csv", row.names = F)
