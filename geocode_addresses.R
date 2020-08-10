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

data <- read_csv(file = file.path(here::here(), "APD_CAD_911_Calls.csv"))

data <- data %>%
  # Not sure what these 1/2's are for
  mutate(clean_address = sub("1/2", "", address)) %>%
  # Separate block, add 50 so it moves to the middle
  # If it's an intersection, use the word "AND" to the API to work (not "&")
  mutate(block_number = as.numeric(sub("\\-BLOCK.*", '', clean_address)) + 50,
         street = sub("/", " AND ", sub(".*-BLOCK", '', clean_address))) %>%
  # If it's an intersection, remove the ampersand
  mutate(clean_address = paste0(block_number, " ", street, ", ASHEVILLE, NC")) %>%
  select(-c(block_number, street))
  
distinct_address <- data %>%
  distinct(clean_address)

test_data <- distinct_address %>%
  top_n(10)

#register_google(key = "your key", write = TRUE)
address_value <- ggmap::geocode(as.character(distinct_address[1, 1]), source = "google")
# just viewing this trucates, click on actual table
# Lon/Lat vs Lat/Lon

address_value <- ggmap::geocode(as.character(	
  "SWEETEN CREEK RD AND PENSACOLA AVE, ASHEVILLE, NC"), source = "google")

test_data_2 <- test_data %>%
  mutate(address_value = purrr::map(clean_address, function(x)
    ggmap::geocode(as.character(x), source = "google"))) %>%
  tidyr::unnest(address_value)


