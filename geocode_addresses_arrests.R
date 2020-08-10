library(tidyverse)
library(here)
library(ggmap)
library(stringr)

## Need to get google API set up

## Disable API's
# https://console.cloud.google.com/google/maps-apis/api-list?project=my-project-1556582735242&folder=&organizationId=&supportedpurview=project
# https://console.cloud.google.com/projectselector2/iam-admin/quotas?supportedpurview=project

# Add Credentials
# Key restrictions, IP addresses
#register_google(key = "", write = TRUE)

data <- read_csv(file = file.path(here::here(), "APD_Arrests.csv"))

data <- data %>%
  # Not sure what these 1/2's are for
  mutate(clean_address = sub("1/2", "", address)) %>%
  # Separate block, add 50 so it moves to the middle
  # If it's an intersection, use the word "AND" to the API to work (not "&")
  mutate(block_number = as.numeric(sub("\\-BLK.*", '', clean_address)) + 50,
         street = sub("/", " AND ", sub(".*-BLK", '', clean_address))) %>%
  # If it's an intersection, remove the ampersand
  mutate(clean_address = paste0(
    ifelse(is.na(block_number), '', block_number), 
    street, ", ASHEVILLE, NC")) %>%
  select(-c(block_number, street))

distinct_address <- data %>%
  distinct(clean_address)

#register_google(key = "your key", write = TRUE)
# address_value_test <- ggmap::geocode(as.character(distinct_address[1, 1]), source = "google")
# just viewing this trucates, click on actual table
# Lon/Lat vs Lat/Lon

# address_value_test_2 <- ggmap::geocode(as.character(	
#   "SWEETEN CREEK RD AND PENSACOLA AVE, ASHEVILLE, NC"), source = "google")
# 
# address_value_test_3 <- ggmap::geocode(as.character(	
#   "9150  DAVIS GREY DR, ASHEVILLE, NC"), source = "google")

# partial_list <- distinct_address %>%
#   top_n(100)
# 
# partial_list <- partial_list %>%
#   mutate(address_value = purrr::map(clean_address, function(x)
#     ggmap::geocode(as.character(x), source = "google"))) %>%
#   tidyr::unnest(address_value)
# 
# write_csv(partial_list,
#           here::here("geolocated_arrests.csv"),
#           append = TRUE)

pre_geolocated <- read_csv(here::here("geolocated_arrests.csv"),
                           col_names = c('clean_address', 
                                         'lon',
                                         'lat'))

broken_geolocated <- read_csv(here::here("broken_arrests_addresses.csv"),
                           col_names = c('clean_address', 
                                         'lon',
                                         'lat'))

distinct_address <- distinct_address %>%
  filter(!clean_address %in% pre_geolocated$clean_address) %>%
  filter(!clean_address %in% broken_geolocated$clean_address)

partial_list <- distinct_address %>%
  top_n(500)

partial_list <- partial_list %>%
  mutate(address_value = purrr::map(clean_address, function(x)
    ggmap::geocode(as.character(x), source = "google"))) %>%
  tidyr::unnest(address_value)

## Save broken addresses
broken_list <- partial_list %>%
  filter(is.na(lat) | is.na(lon))

write_csv(broken_list,
          here::here("broken_arrests_addresses.csv"),
          append = TRUE)

## Save good addresses
partial_list <- partial_list %>%
  filter(!(is.na(lat) | is.na(lon)))

write_csv(partial_list,
          here::here("geolocated_arrests.csv"),
          append = TRUE)
