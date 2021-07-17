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
library(zoo)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")

# Minimum wage ---- 

## read in data

min_wage_pt1 = read_excel("state_min_wage.xls", sheet = 'Annual_1')
min_wage_pt2 = read_excel("state_min_wage.xls", sheet = 'Annual_2')

state_med_inc = read_excel("state_med_income.xls", sheet = "Annual")

state_unemp = read_excel("state_unemp.xls", sheet='Annual')
state_unemp_key = read_excel("state_unemp.xls", sheet='README') 

years = c(2010:2019)

hate_data = lapply(years, function(x) (read.csv(sprintf("splc_hate_data/splc-hate-groups-%s.csv", x), 
                                                stringsAsFactors = F))) %>%
  bind_rows()

state_educ = read_excel("state_education.xls", sheet = "Annual")

read_and_clean_pop <- function(state) {
  
  print(state)
  
  state_data = read_excel(sprintf("state_pop/%s_state_pop.xlsx", state))
  state_data$state = state
  
  state_data = state_data %>%
    pmap_dfr(., ~ na.locf(c(...)) %>%
               as.list %>%
               as_tibble)
  
  colnames(state_data) = lapply(c(1:length(colnames(state_data))), function(i) (paste(state_data[4,i], state_data[3,i]))) %>%
    unlist()
  
  colnames(state_data) = c("Age", "Total Population Estimates Census", 
                           colnames(state_data)[2:36],
                           colnames(state_data)[38:39])
  state_data = state_data[5:91,]
  
  state_data$Age = c("Total", 0:85)
  
  state_data = state_data %>%
    mutate(Age = as.numeric(Age),
           age_group = ifelse(Age < 18, "Below 18", NA),
           age_group = ifelse((Age >= 18 & Age < 34), "18 to 34", age_group),
           age_group = ifelse((Age >= 34 & Age < 56), "34 to 56", age_group),
           age_group = ifelse((Age >= 56 & Age < 65), "56 to 65", age_group),
           age_group = ifelse(Age >= 65, "65 plus", age_group))
  
  state_data = state_data[,c(1, 8:40)] %>%
    select(-c("Total\r\nPopulation Census"))
  
  temp = data.frame()
  temp_state_varname = paste(state, state)
  
  for(i in 2010:2019) {
    year_dat = state_data %>%
      select(Age, contains(as.character(i)), c(age_group, temp_state_varname)) %>%
      mutate(year = i)
    
    colnames(year_dat) = c("Age", "Total", "Male", "Female",
                           "age_group","state", "year")
    
    temp = rbind(temp, year_dat)
  }
  
  state_data = temp
  
  state_data = state_data %>%
    group_by(age_group, year, state) %>%
    mutate(Male = as.numeric(Male),
           Female = as.numeric(Female),
           Total = as.numeric(Total)) %>%
    summarise(num_males = sum(Male, na.rm = T), 
              num_females = sum(Female, na.rm = T),
              num_total = sum(Total, na.rm = T))
  
  temp = state_data[51:60,] %>%
    rename("total_males" ="num_males", 
           "total_females" = "num_females", 
           "total" = "num_total") %>%
    ungroup() %>%
    select(-c(age_group))
  
  state_data = state_data %>%
    right_join(temp, by = c("year", "state"))
  
  state_data = state_data[1:50,]
  
  state_data = state_data %>% 
    group_by(year, age_group, state) %>%
    mutate(pct_age_group = num_total/total) 
  
  temp = state_data %>% 
    spread(key = age_group,
           value = pct_age_group) 
  
  temp = temp %>% 
    arrange(`18 to 34`) %>%
    mutate(`18 to 34` = `18 to 34`[1])
  
  temp = temp %>% 
    arrange(`Below 18`) %>%
    mutate(`Below 18` = `Below 18`[1])
  
  temp = temp %>% 
    arrange(`34 to 56`) %>%
    mutate(`34 to 56` = `34 to 56`[1])
  
  temp = temp %>% 
    arrange(`56 to 65`) %>%
    mutate(`56 to 65` = `56 to 65`[1])
  
  temp = temp %>% 
    arrange(`65 plus`) %>%
    mutate(`65 plus` = `65 plus`[1])
  
  state_data = temp %>%
    group_by(year, state) %>%
    summarise(pct_male = total_males/total,
              pct_female = total_females/total,
              total_pop = total,
              `Below 18` = `Below 18`,
              `18 to 34` = `18 to 34`,
              `34 to 56` = `34 to 56`,
              `56 to 65` = `56 to 65`,
              `65 plus` = `65 plus`)%>%
    unique()
  
  return(state_data)
}

state_pop = lapply(state.abb, read_and_clean_pop) %>%
  bind_rows()

hate_data = lapply(2010:2019, 
                   function(year) (read.csv(sprintf("splc_hate_data/splc-hate-groups-%s.csv", year)))) %>%
  bind_rows()

read_and_clean_race_data = function(table) {
  
  print(table)
  
  data = read_excel("census_race_data.xlsx", sheet = sprintf("Tabulate %s - Table 1", table))
  
  colnames(data) = c("race", "total", data[6,3:53])
  
  data = data[7:14,]
  
  data = data[c(1:2, 4:8),]
  data$race[2] = "White"
  
  data = t(data) %>%
    as.data.frame()
  colnames(data) = data[1,]
  data = data[3:53,]
  data$state = rownames(data)
  data$year = 2011 + table
  
  data[,1:7]= data[,1:7] %>% 
    mutate_if(is.character, as.numeric)
  
  colnames(data) = c("Totals", "White", "Black or African American\r\nalone",
                     "American Indian and Alaska\r\nNative alone",
                     "Asian alone", "Native Hawaiian and Other\r\nPacific Islander alone",
                     "Two or more races", "state", "year")
  
  data = data %>%
    rowwise() %>%
    mutate(pct_white = White/Totals,
           pct_black = `Black or African American\r\nalone`/Totals,
           pct_AIAN = `American Indian and Alaska\r\nNative alone`/Totals,
           pct_asian = `Asian alone`/Totals, 
           pct_NHPI = `Native Hawaiian and Other\r\nPacific Islander alone`/Totals,
           pct_two_plus = `Two or more races`/Totals) %>%
    select(year, state, pct_white, pct_black, pct_AIAN, pct_asian, pct_NHPI,
           pct_two_plus)
  
  data[is.na(data)] <- 0
  
  return(data)
}

race_data = lapply(1:7, read_and_clean_race_data) %>%
  bind_rows()

## clean 

### clean up names
min_wage = cbind(min_wage_pt1, min_wage_pt2) %>% select("observation_date",contains("20210104"))
colnames(min_wage) = gsub("STTMINWG","", colnames(min_wage))
colnames(min_wage) = gsub("_20210104","", colnames(min_wage))

colnames(state_med_inc) = gsub("MEHOINUS","", colnames(min_wage))
colnames(state_med_inc) = gsub("A646N","", colnames(min_wage))

state_med_inc = state_med_inc %>%
  gather(key = "state",
         value = "med_inc",
         -observation_date) %>%
  mutate(observation_date = year(observation_date)) %>%
  filter(observation_date >= 2009) %>%
  rename("year" = "observation_date")

colnames(state_educ) = gsub("GCT1501","hs_", colnames(state_educ))
colnames(state_educ) = gsub("GCT1502","bach_", colnames(state_educ))
colnames(state_educ) = gsub("GCT1503","gpd_", colnames(state_educ))

state_educ = state_educ %>%
  as.data.table() %>%
  melt(id.vars = "DATE",
       measure.vars = 2:151) %>%
  as.data.frame() %>%
  mutate(state = sub("^[^_]*", "", variable),
         state = substr(state, 2, 3),
         variable = sub("*_.*", "", variable)) %>%
  spread(key = "variable",
         value = "value") %>%
  mutate(hs = hs - bach,
         bach = bach - gpd,
         total = hs + bach + gpd,
         DATE = year(DATE) - 1) %>% #matching it to year that it's already been recorded
  rename("date" = "DATE")

hate_data = hate_data %>%
  select(c("Title", "State", "Year")) %>%
  group_by(State, Year) %>%
  summarise(num_hate_groups = n()) %>%
  mutate(Year = paste0(Year, "-01-01"),
         Year = as.Date(Year, "%Y-%m-%d")) %>%
  complete(Year = seq.Date(as.Date("2010-01-01"), as.Date("2019-01-01"), by = "year")) %>%
  filter(State != "District of Columbia") %>%
  mutate(num_hate_groups = ifelse(is.na(num_hate_groups), 0, num_hate_groups),
         state = state.abb[which(state.name == State)], 
         Year = year(Year)) %>%
  ungroup() %>%
  select(year = Year, state, num_hate_groups)


### the one for state level unemployment is a little more involved, since 
## abbr. not embedded in varname
colnames(state_unemp_key) = c("temp")
state_unemp_key = state_unemp_key[grepl("LAUST", state_unemp_key[["temp"]]) | 
                        grepl("Unemployment Rate in ", state_unemp_key[["temp"]]),]

sorter_nums = rep(c("varname","varstate"), 50)
state_unemp_key$var = sorter_nums
state_ids = sort(rep(1:50, 2))
state_unemp_key$state_id = state_ids

key = state_unemp_key %>% 
  pivot_wider(names_from = "var", values_from = "temp") %>%
  mutate(varstate = gsub("Unemployment Rate in ", "", varstate),
         varstate = state.abb[which(state.name == varstate)]) %>%
  select(-c("state_id"))

#now renaming the colnames to coincide with the key from the readme
colnames(state_unemp) = colnames(state_unemp) %>%
  recode(!!!setNames(as.character(key$varstate), key$varname))

state_unemp = state_unemp %>%
  gather(key = "state",
         val = "unemp_rate",
         -DATE) %>%
  select("year" = "DATE", "state", "unemp_rate") %>%
  mutate(unemp_rate = unemp_rate/100,
         year = as.numeric(as.character(year(year))))

### clean up dates
min_wage = min_wage %>% 
  mutate(observation_date = gsub(" UTC", "", observation_date),
         observation_date = substr(observation_date,1,4)) %>%
  filter(observation_date >= 2010)

#adding in states which have min wage = fed min wage
min_wage$AL = 7.25
min_wage$LA = 7.25
min_wage$MS = 7.25
min_wage$SC = 7.25
min_wage$TN = 7.25

#### wide to long

min_wage_l = min_wage %>% 
  gather(state,min_wage,AK:TN)

## mark if states didn't change from 2009 to 2016
min_wage_l = min_wage_l %>%
  group_by(state) %>%
  mutate(yoy_change = min_wage/lag(min_wage),
         ten_year_change = min_wage/lag(min_wage, 10),
         three_year_change = min_wage/lag(min_wage, 3)) %>%
  mutate(treat = ifelse((observation_date >= 2014 & 
                        observation_date <= 2016 &
                        yoy_change >= 1.05), 
                        1, 0), 
         control = ifelse((ten_year_change == 1 &
                             observation_date == 2020), 1, 0)) %>%
  mutate(treat = max(treat, na.rm = T),
         control = max(control, na.rm = T)) %>%
  rename("year" = "observation_date")

#### Writing Out ----

controls = min_wage_l %>%
  mutate(year = as.numeric(year)) %>%
  left_join(hate_data, by = c("year", "state")) %>%
  left_join(state_unemp, by = c("year", "state")) %>%
  left_join(state_pop, by = c("year", "state")) %>%
  left_join(state_educ, by = c("year" = "date", "state")) %>%
  left_join(state_med_inc, by = c("year", "state"))%>%
  left_join(race_data, by = c("year", "state"))

write.csv(controls, "controls.csv", row.names = F)
