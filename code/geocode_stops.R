# Code to geocode & save the stops dataset from the route census. 

# This code: 
#  * Reads in the stops dataset from Google Drive 
#  * Queries OSM within a reasonable search radius (bounding box defined by the extent of all route geometries) 
#     for the names of each stop in the stops datasets
#  * Returns the coordinates of each stop from OSM 
#  * Snaps the returned coordinate to the corresponding route 
 


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
  # About: Dataset stop_dataset.csv has more than one row per stop. Appears to have 1 row per stop X year (2000-2024) X respondent combination  
  # Uniquely identified by: ??? 

stops_raw <- vroom(file.path(drive_path, "dataprocessed/historical-census/csv", "stop_dataset.csv")) 
problems(stops_raw)

# label vars 
attr(stops_raw$r2, "label") <- "Name of Main Stop"
attr(stops_raw$r3, "label") <- "In which year did the branch start servicing ${R2}?"
attr(stops_raw$r8_diff, "label") <- "Still on ${R2}, please fill out the history of passenger fares / prices on this route to ${R2}."

# Dataset only containing stops information 
stops <- stops_raw %>% 
  # Create a unique identifier for later use
  mutate(unique_id = row_number()) %>% 
  rename(id = ObjectID_1) %>% 
  distinct(id, r2, route_id, stage_id, branch_id, taxi_park, r3, r8_diff, geometry, .keep_all=T) %>%
  st_as_sf(wkt = 'geometry') %>% 
  filter(id!=0)

st_crs(stops) <- 3857   # Web Mercator  - this looks right when mapped 




# Geocode Stops  ----------------------------------------------------------------------

# Try with a sample of just a few stops
stops_test <- stops[1:100,] %>% 
  st_transform(4326)  # transform to WGS84 coordinates 

# Bounding box (all routes)
bbox <- st_bbox(stops_test)
  
# Query 
stops_geocoded_q <- stops_test %>% 
  geocode(address = r2, method = "osm", lat = latitude, long = longitude,
          custom_query = list(
            viewbox = paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ","),
            bounded = 1),
          full_results = TRUE)

# Set geometry to the coordinates fetched, keeping only those for which query returned coords
stops_geocoded <- stops_geocoded_q %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  unique()


# Calculate success rate 
n_searchedfor <- distinct(stops_test, r2) %>% nrow
n_found <- nrow(stops_geocoded)

print(paste0(n_found, " out of ", n_searchedfor, " distinct stops located."))


# View results: 
routes_map <- tm_shape(stops, is_main = FALSE,
                       name = "Routes") + 
  tm_lines(lwd=0.5, col_alpha=0.2) 

stops_map <- tm_shape(stops_geocoded %>% select(r2, name, route_id),
                      name = "Stops - OSM location") + 
  tm_dots(size = .7, fill = 'red', fill_alpha = .5, hover = 'name')

tm_basemap("OpenStreetMap") + 
  routes_map + stops_map + 
  tm_title("OSM-identified stop locations vs All routes")



# Snap stop coordinates to the route geometries ---------------------------------------

# Dataframe with ONLY the routes
stops_routes_geos <- stops %>% 
  distinct(id, r2, geometry, .keep_all=TRUE) %>% 
  filter(unique_id %in% stops_geocoded$unique_id) %>% 
  arrange(unique_id) %>% 
  unique()

# Transform stop coordinates to match 
stops_geocoded_2 <- stops_geocoded %>% st_set_crs(4326) %>% 
  st_transform(crs = st_crs(stops)) %>% 
  arrange(unique_id)

# Find nearest points along the routes to the stops 
nearest_lines <- st_nearest_points(stops_routes_geos[1:50,], stops_geocoded_2[1:50,], pairwise=T)
snapped_points <- st_cast(nearest_lines, "POINT", group_or_split=F) %>% 
  st_as_sf() %>% 
  mutate(r2 = stops_routes_geos[1:50,]$r2)

# view 
tm_basemap("OpenStreetMap") + 
tm_shape(stops_geocoded_2 %>% select(r2, route_id, stage_id, stage_name, branch_id, taxi_park), 
         name = "Stops (OSM-returned coordinates)", hover='r2') + tm_dots(size=.7) + 
tm_shape(stops_routes_geos %>% select(year, stage_id, stage_name), 
         name = "Routes", hover = 'id') + tm_lines() + 
tm_shape(nearest_lines) + tm_lines(col='red') +
tm_shape(snapped_points, name = "Stops - snapped locations") + tm_dots(size=.7, fill='red') +
tm_title("Stop locations: Snapped to routes")

