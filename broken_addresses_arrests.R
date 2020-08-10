library(tidyverse)
library(here)
library(ggmap)
library(stringr)
library(leaflet)
library(ggplot2)

pre_geolocated <- read_csv(here::here("geolocated_arrests.csv"),
                           col_names = c('clean_address', 
                                         'lon',
                                         'lat'))

broken_geolocated <- read_csv(here::here("broken_arrests_addresses.csv"),
                              col_names = c('clean_address', 
                                            'lon',
                                            'lat'))

ggplot(data = pre_geolocated,
       aes(lon, lat)) +
  geom_point()
## Some are definitely wrong


## Asheville is 35.5951, -82.5515
library(sf)
library(maps)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot(data = states) +
  geom_sf() +
  geom_point(data = pre_geolocated, aes(x = lon, y = lat))

library(geosphere)

pre_geolocated$dist_to_asheville <- distm(as.matrix(pre_geolocated[, 2:3]), 
                                          c(-82.5515, 35.5951)) * .000621371

ggplot(data = states) +
  geom_sf() +
  geom_point(data = pre_geolocated, aes(x = lon, y = lat, col = dist_to_asheville))

# drive from Barnardsville to Asheville is 20 mi
# So say anything past 30

pre_geolocated_too_far <- pre_geolocated %>%
  filter(dist_to_asheville >= 30)

pre_geolocated_cleaned <- pre_geolocated %>%
  filter(dist_to_asheville < 30)

pre_geolocated_cleaned_count <- pre_geolocated_cleaned %>%
  group_by(lon, lat) %>%
  mutate(num_addresses = n_distinct(clean_address),
         group_id = cur_group_id())

pre_geolocated_too_many <- pre_geolocated_cleaned_count %>%
  filter(num_addresses >= 5)

broken_geolocated <- rbind(broken_geolocated,
                           pre_geolocated_too_far[, 1:3],
                           pre_geolocated_too_many[, 1:3])

# write_csv(broken_geolocated,
#           here::here("broken_odd_geolocated.csv"))






             