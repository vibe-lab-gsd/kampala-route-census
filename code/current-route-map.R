library(tidyverse)
library(sf)
library(here)
library(rnaturalearth)
library(osmextract)
library(ggspatial)

lake_victora <- ne_download(type = "lakes",
                            category = "physical",
                            scale = 10) |>
  filter(name == "Lake Victoria")

roads <- osmextract::oe_get(place = "Uganda")

roads <- roads |>
  filter(!is.na(highway)) |>
  filter(highway %in% c("primary",
                        "secondary"))

taxi_routes <- here("route-data",
                    "routes_final") |>
  st_read() |>
  filter(F2024 == 1) 

taxi_routes <- taxi_routes |>
  mutate(id = 1:nrow(taxi_routes))

bbox <- st_bbox(taxi_routes)

OTP <- here("kampala-geography",
            "OTP-bounary.kml") |>
  st_read()

kito <- here("route-data",
             "20250905-Kito.kml") |>
  st_read() |>
  filter(Name == "Track 20250905-140324")

taxi_routes_otp <- taxi_routes[OTP,]

non_otp_routes <- taxi_routes |>
  filter(!id %in% taxi_routes_otp$id)

ggplot(non_otp_routes) +
  geom_sf(data = roads,
          aes(color = "Primary and secondary roads")) +
  geom_sf(data = non_otp_routes,
          aes(color = "Taxi routes originating outside Old Taxi Park"),
          linewidth = 0.5) +
  geom_sf(data = taxi_routes_otp,
          aes(color = "Taxi routes originating within Old Taxi Park"),
          linewidth = 0.75) + 
  geom_sf(data = kito,
          aes(color = "Kito route"),
          linewidth = 1.5) +
  geom_sf(data = lake_victora,
          color = NA,
          fill = "darkslategray3") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  scale_color_manual(name = "Legend",
                     values = c("firebrick3",
                                "lightgray",
                                "burlywood",
                                "burlywood4")) +
  annotation_scale() +
  theme_void()

here("figures",
     "current-route-map.pdf") |>
  ggsave(width = 12, height = 7.5,
         units = "in")
