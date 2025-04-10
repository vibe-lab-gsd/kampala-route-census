library(sf)
library(tidyverse)
library(stars)

pop_data_2020_url <- 
  "https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/UGA/uga_ppp_2020.tif"

### Note that this next part takes about 25 minutes to run.
pop_data_2020 <- stars::read_stars(pop_data_2020_url) |>
  st_as_sf(as_points = TRUE, merge = FALSE) |>
  st_filter(service_neighborhoods) |>
  st_transform(32736)

grid_pop <- pop_data_2020 |>
  st_join(grid) |>
  st_drop_geometry() |>
  group_by(id) |>
  summarise(pop_2020 = sum(uga_ppp_2020.tif))

neighborhoods_pop <- pop_data_2020 |>
  st_join(service_neighborhoods) |>
  st_drop_geometry() |>
  group_by(shapeName) |>
  summarize(pop_2020 = sum(uga_ppp_2020.tif))