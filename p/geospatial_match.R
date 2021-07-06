# Geospatial Matching
# July 2021
# 
# This file takes the mildly cleaned twitter and adds lat long pulled from twitter
# to get a general idea of which state tweets originate from. 
# 
# 
# previous script: 
#   twitter_clean.R
#   place_id_to_lat_long.py
# 
# inputs: 
#   undup_tweets.csv
#   place_geo_data2.csv
#   
# outputs: 
#   tweets_final.csv


#### Setting up environment & pulling data ---- 

library(sf)
library(spData)
library(data.table)
library(tidygeocoder)
library(parallel)
library(ggmap)
library(tidyverse)

setwd("C:/Users/unnav/Dropbox/Coding/Inequality-and-Hate-Speech/d")


##### Import and clean ---- 

tweets = fread("undup_tweets.csv", stringsAsFactors = F) %>%
  select(-c(V1))
place_geo_data = fread("place_geo_data2.csv", stringsAsFactors = F)

place_geo_data_w = reshape(place_geo_data, 
                         idvar = "geocode",
                         timevar = "V1",
                         direction = "wide") %>%
  select(-c(name.1)) %>%
  select(geocode, name = name.0, long = centroid.0, lat = centroid.1)

##### Adding Geospatial Data ----

#add place_ids long lat
geo_tweets = tweets %>% left_join(place_geo_data_w, by = c("geo.place_id" = "geocode")) %>%
  filter(!(geo.place_id == "" & geo.coordinates.coordinates == ""))

# get existing lat long if they are there
geo_tweets = geo_tweets %>%
  mutate(geo.coordinates.coordinates = str_replace(geo.coordinates.coordinates, "\\[", ""),
         geo.coordinates.coordinates = str_replace(geo.coordinates.coordinates, "\\]", "")) %>%
  separate(geo.coordinates.coordinates, c("coord_long", "coord_lat"), ", ")

#only want to use no more precise long lat is available  
geo_tweets = geo_tweets %>%
    mutate(long = ifelse(coord_long != "" & !is.na(coord_long), coord_long, long),
           lat = ifelse(coord_lat != "" & !is.na(coord_lat), coord_lat, lat)) %>% #in theory these tests should equal the same thing
   select(-c(coord_long, coord_lat)) 

#only keep what i have location data for
geo_tweets = geo_tweets %>% filter(lat != "", long != "")%>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))

rm(tweets)

##### Matching state info -----
## taken from https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r

## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC.
##
## name_col: Name of a column in `states` that supplies the states'
##           names.

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}


#an ugly implementation but time is money

long_lat_df = geo_tweets %>% select(long, lat) %>%
  unique()
  
long_lat_df$state = lonlat_to_state(long_lat_df)

geo_tweets = geo_tweets %>%
  right_join(long_lat_df, by = c("long", "lat"))

fwrite(geo_tweets, "tweets_final.csv", row.names = F)
