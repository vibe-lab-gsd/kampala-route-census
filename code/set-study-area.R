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
  st_read()

service_neighborhoods <- neighborhoods |>
  st_filter(service_area) |>
  st_transform(32736)

grid <- st_make_grid(service_neighborhoods,
                     cellsize = as_units(500, "m"),
                     square = FALSE) |>
  st_as_sf() 

grid <- grid |>
  rename(geometry = x) |>
  mutate(id = seq(1, nrow(grid), by=1)) |>
  select(id, geometry) |>
  st_filter(service_neighborhoods)

st_write(grid, 
         here("kampala-geography",
              "study-area-grid-cells.geojson"))

st_write(service_neighborhoods, 
         here("kampala-geography",
              "study-area-neighborhoods.geojson"))



