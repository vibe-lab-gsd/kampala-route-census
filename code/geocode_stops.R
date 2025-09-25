# Code to geocode & save the stops dataset from the route census. 

library(haven)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(survival)
library(osmdata)
library(vroom)
library(sf)
library(tmap)
library(eeptools)
library(tidygeocoder)


# DIRECTORY ----------------------------------------------------------------------
git_path <-  "C:/Users/Gray Collins/Documents/GitHub/kampala-route-census"
drive_path <-  "G:/Shared drives/ugandatransit/uganda_transit_archives"


# Route dataset  ----------------------------------------------------------------------
routes_a_raw <- vroom(file.path(git_path, "route-data", "route_dataset_anonymized.csv"))
problems(routes_a_raw)

isid(routes_a_raw, c("route_id", "year"), verbose=T)



# Read stops data  ----------------------------------------------------------------------
stops_raw <- vroom(file.path(drive_path, "dataprocessed/historical-census/csv", "stop_dataset.csv")) 
problems(stops_raw)

# label vars 
attr(stops_raw$r2, "label") <- "Name of Main Stop"
attr(stops_raw$r3, "label") <- "In which year did the branch start servicing ${R2}?"
attr(stops_raw$r8_diff, "label") <- "Still on ${R2}, please fill out the history of passenger fares / prices on this route to ${R2}."

# Dataset only containing stops information 
stops <- stops_raw %>% 
  rename(id= ObjectID_1) %>% 
  distinct(id, r2, route_id, stage_id, branch_id, taxi_park, r3, r8_diff, geometry) %>%
  st_as_sf(wkt = 'geometry') %>% 
  filter(id!=0)

st_crs(stops) <- 3857   # Web Mercator  - this looks right when mapped 


# note - id in the stops data is the unique routexyear identifier
routes <- stops_raw %>% 
  distinct(route_id, geometry, .keep_all=T) %>% 
  select(route_id, year, geometry)

st_as_sf(wkt = 'geometry')  
  



# Geocode Routes  ----------------------------------------------------------------------

# Try with a sample of just 10 stops
stops_test <- stops[1:100,]
stops_test2 <- st_transform(stops_test, 4326)  # transform to WGS84 coordinates 

# Bounding box (all routes)
bbox <- st_bbox(stops_test2)

# Query 
stops_geocoded_q <- stops_test2 %>% 
  geocode(address = r2, method = "osm", lat = latitude, long = longitude,
          custom_query = list(
            viewbox = paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ","),
            bounded = 1),
          full_results = TRUE)

# Set geometry to the coordinates fetched, keeping only those for which query returned coords
stops_geocoded <- stops_geocoded_q %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"))

# Calculate success rate 
n_searchedfor <- distinct(stops[1:100,], r2) %>% nrow
n_found <- nrow(stops_geocoded)

print(paste0(n_searchedfor, " out of ", n_found, "distinct stops located."))

# View results: 
routes_map <- tm_shape(stops) + 
  tm_lines(lwd=0.5, col_alpha=0.2) 

stops_map <- tm_shape(stops_geocoded %>% select(r2, name, route_id)) + 
  tm_dots(size = 1, col = 'red', col_alpha = .5, hover = 'name')

tm_basemap("OpenStreetMap") + 
  routes_map + stops_map 


