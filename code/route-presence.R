library(tidyverse)
library(sf)
library(here)

routes <- here("route-data",
               "routes_final",
               "routes.shp") |>
  st_read() |>
  st_transform(32736)

grid <- here("kampala-geography",
             "study-area-grid-cells.geojson") |>
  st_read() |>
  st_transform(32736) 

neighborhoods <- here("kampala-geography",
                      "study-area-neighborhoods.geojson") |>
  st_read() |>
  st_transform(32736) 

for (i in 2000:2024) {
  this_colname <- paste0("F", i)
  
  these_routes <- routes |>
    filter(!!sym(this_colname) == 1)
    
  neighborhoods <- neighborhoods |>
    mutate(!!sym(this_colname) := 
             lengths(st_intersects(neighborhoods, these_routes)))
  
  grid <- grid |>
    mutate(!!sym(this_colname) := 
             lengths(st_intersects(grid, these_routes)))
  
}

st_write(grid, 
         here("kampala-geography",
              "grid_routes.geojson"))

st_write(neighborhoods, 
         here("kampala-geography",
              "neighborhood-routes.geojson"))
