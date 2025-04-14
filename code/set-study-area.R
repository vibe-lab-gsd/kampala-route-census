library(sf)
library(tidyverse)
library(here)
library(units)

routes <- here("route-data",
               "routes_final",
               "routes.shp") |>
  st_read()

service_area <- st_union(routes) |>
  st_convex_hull() 

# ADM 4 boundaries from 
# https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-uganda
neighborhoods <- here("kampala-geography",
                      "geoBoundaries-UGA-ADM4.geojson") |>
  st_read() |>
  st_filter(service_area) |>
  st_transform(32736) |>
  mutate(name = shapeName) |>
  select(name)

grid <- st_make_grid(neighborhoods,
                     cellsize = as_units(1, "km"),
                     square = FALSE) |>
  st_as_sf() 

grid <- grid |>
  mutate(id = seq(1, nrow(grid), by=1)) 

grid_nhoods <- st_centroid(grid) |>
  st_filter(neighborhoods) |>
  st_join(neighborhoods) |>
  st_drop_geometry()

grid <- grid |>
  right_join(grid_nhoods) |>
  rename(geometry = x,
         neighborhood = name) |>
  select(id, neighborhood, geometry) 

st_write(grid, 
         here("kampala-geography",
              "study-area-grid-cells.geojson"))

st_write(neighborhoods, 
         here("kampala-geography",
              "study-area-neighborhoods.geojson"))



