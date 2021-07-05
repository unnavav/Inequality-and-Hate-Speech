library(sf)
library(spData)



temp = tweets %>% mutate(geo.coordinates.coordinates = str_replace(geo.coordinates.coordinates, "\\[", ""),
                         geo.coordinates.coordinates = str_replace(geo.coordinates.coordinates, "\\]", "")) %>%
  separate(geo.coordinates.coordinates, c("long", "lat"), ", ")


# matching state info
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

temp = tweets %>% mutate(state = lonlat_to_state(data.frame(long, lat)))
