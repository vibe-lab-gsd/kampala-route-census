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



# NEW: Stops dataset = 1 row per stop
  stops_raw %>% 
    distinct(r2, r3, year, route_id, stage_id, branch_id, taxi_park) %>% 
    mutate(year = as.numeric(year), 
           r3 = as.numeric(r3)) %>% 
    filter(year==r3 | (r3<2000 & year==2000))  # Keep earliest obs 

# list of unique stop names: (w/cleaning)
stop_names <- stops_raw %>% 
  mutate(
    r2 = tolower(r2) %>% 
      str_replace_all(c("center"="centre"))
  ) %>% 
  distinct(r2)


# Geocode Stops  ----------------------------------------------------------------------

# Try all, or filter `stops` to try on a subset of just a few stops
stops_test <- stops %>% 
  st_transform(4326)  # transform to WGS84 coordinates 

# Bounding box (all routes)
bbox <- st_bbox(st_transform(stops, 4326))

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
n_searchedfor <- nrow(stops_test)
n_found <- nrow(stops_geocoded_q[!is.na(stops_geocoded_q$place_id), ])
print(paste0("Matches by place name: ", n_found, " out of ", n_searchedfor, " stops located."))

# List of unmatched stops: 
stops_geocoded_q %>%
  filter(is.na(place_id)) %>% 
  distinct(r2, route_id, stage_id, branch_id, taxi_park) %>% 
  view


# View results: 
routes_map <- tm_shape(stops, is_main = FALSE, name = "Routes") + 
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

stopifnot(stops_routes_geos$unique_id==stops_geocoded_2$unique_id)

# Find nearest points along the routes to the stops & # Filter by distance threshold
thresh <- 500 # meters 

nearest_lines <- st_nearest_points(stops_routes_geos, stops_geocoded_2, pairwise=T)
snapped_points <- st_cast(nearest_lines, "POINT", group_or_split=F) %>% 
  st_as_sf() %>% 
  mutate(r2 = stops_routes_geos$r2,
         dist_to_route = st_length(nearest_lines) %>% round() %>% as.numeric()) %>% 
  filter(dist_to_route<=thresh)

nearest_lines_df <- st_length(nearest_lines) %>% round() %>% 
  as.numeric() %>% 
  as.data.frame() %>% 
  rename("dist_to_route" = ".") %>% 
  st_set_geometry(nearest_lines) %>% 
  filter(dist_to_route<=thresh)

print(paste0("When filtering to points within ", thresh, " meters of a route: ", 
             nrow(nearest_lines_df), " out of ", n_searchedfor, " stops located."))


# view 
resmap <- tm_basemap("OpenStreetMap") + 
  tm_shape(stops_geocoded_2 %>% select(unique_id, r2, name, route_id, stage_id, stage_name, branch_id, taxi_park), 
           name = "Stops (OSM-returned coordinates)", hover='r2') + tm_dots(size=.7) + 
  tm_shape(stops_routes_geos %>% select(unique_id, route_id, stage_id, stage_name, branch_id, taxi_park), 
           name = "Routes", hover='route_id') + tm_lines(col='black', lwd = 1) + 
  tm_shape(nearest_lines_df) + tm_lines(col='red') +
  tm_shape(snapped_points, is.main = TRUE, name = "Stops - snapped locations") + tm_dots(size=.7, fill='red') +
  tm_title("Stop locations: Snapped to routes")

tmap_mode("view")
resmap

nearest_lines[st_length(nearest_lines)==max(st_length(nearest_lines)),] %>% 
  tm_shape() + tm_lines(col = 'purple') + 
  routes_map + stops_map

stops_routes_geos[800,]
stops_geocoded_2[800,]

tmap_save(tm = resmap, 
          filename = file.path(git_path, "code", "geocode_stops_map.html"))




