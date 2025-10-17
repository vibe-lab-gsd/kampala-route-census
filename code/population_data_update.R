library(sf)
library(tidyverse)
library(stars)
library(here)

pop_files <- paste0("uga_ppp_",
                    2000:2024,
                    ".tif")

pop_data_urls <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/",
                        2000:2024,
                        "/UGA/",
                        pop_files)

pop_vars <- paste0("pop_", 2000:2024)

### Note that this next part takes about 25 minutes to run.
pop_data <- stars::read_stars(pop_data_urls[1]) |>
  st_as_sf(as_points = TRUE, merge = FALSE) 

grid <- here("kampala-geography",
             "grid_routes.geojson") |>
  st_read() |>
  st_transform(st_crs(pop_data))

neighborhoods <- here("kampala-geography",
                      "neighborhood-routes.geojson") |>
  st_read() |>
  st_transform(st_crs(pop_data))

colnames(pop_data) <- c("population", "geometry")

pop_grid_nhood <- pop_data |>
  st_join(grid) |>
  st_drop_geometry() |>
  filter(!is.na(id))

grid_pop <- pop_grid_nhood |>
  group_by(id) |>
  summarise(pop_2000 = sum(population))

nhood_pop <- pop_grid_nhood |>
  group_by(neighborhood) |>
  summarise(pop_2000 = sum(population))

for(i in 2:length(pop_data_urls)) {
  pop_data <- stars::read_stars(pop_data_urls[i]) |>
    st_as_sf(as_points = TRUE, merge = FALSE) 
  
  colnames(pop_data) <- c("population", "geometry")
  
  pop_grid_nhood <- pop_data |>
    st_join(grid) |>
    st_drop_geometry() |>
    filter(!is.na(id))
  
  next_grid_pop <- pop_grid_nhood |>
    group_by(id) |>
    summarise(pop = sum(population))
  
  colnames(next_grid_pop) <- c("id", paste0("pop_", i+1999))
  
  next_nhood_pop <- pop_grid_nhood |>
    group_by(neighborhood) |>
    summarise(pop = sum(population))
  
  colnames(next_nhood_pop) <- c("neighborhood", paste0("pop_", i+1999))
  
  grid_pop <- full_join(grid_pop, next_grid_pop)
  nhood_pop <- full_join(nhood_pop, next_nhood_pop)
  
  write_csv(grid_pop,
            here("population-data",
                 "grid-pop.csv"))
  
  write_csv(nhood_pop_pop,
            here("population-data",
                 "nhood-pop.csv"))
}

