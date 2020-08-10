##---------
# Libraries
##---------
library(tidyverse)
library(here)
library(ggplot2)
library(colorspace)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(tidycensus)

##----------
# Data Setup
##----------
data <- read_csv(file = file.path(here::here(), "APD_Arrests.csv"))
head(data)

data_summary <- data %>%
  group_by(subject_race, subject_gender) %>%
  summarise(count = n())
data_summary
# No Hispanic

##---------
# Law Beats
##---------
## https://data-avl.opendata.arcgis.com/datasets/
#  0710dd841bea4c9285fe2cbddf89409f_0
law_beats <- st_read(here::here("Law_Beats/Law_Beats.shp"))

ggplot() +
  geom_sf(data = law_beats, aes(fill = beat)) +
  coord_sf()

# Place on map
asheville_map <- get_googlemap(c(lon = -82.5547841,
                                 lat = 35.5229617), # Biltmore
                               zoom = 11,
                               maptype = "roadmap") %>%
  ggmap()

# https://stackoverflow.com/questions/18084609
# /im-having-trouble-adding-a-shapefile-to
# -my-ggmap-due-to-differing-geographic-un
law_beats_sp <- as(law_beats, "Spatial")
law_beats_sp <- spTransform(law_beats_sp, CRS("+proj=longlat +datum=WGS84"))
law_beats_sp <- fortify(law_beats_sp)
law_beats_sp$id <- law_beats$beat[as.numeric(law_beats_sp$id)]

asheville_map +
  geom_polygon(data = law_beats_sp, aes(x = long, y = lat,
                                        group = group, fill = id),
               colour = "grey", alpha = .4, size = .1) +
  coord_equal() +
  coord_map()

##-----------
# Census Info
##-----------

## See variables
acs_vars <- c(
  # RACE
  "B02001_001E", # Estimate!!Total
  "B02001_002E", # Estimate!!Total!!White alone
  "B02001_003E", # Estimate!!Total!!Black or African American alone
  "B02001_004E", # Estimate!!Total!!American Indian and Alaska Native alone
  "B02001_005E", # Estimate!!Total!!Asian alone
  "B02001_006E", # Estimate!!Total!!Native Hawaiian and
                 #   Other Pacific Islander alone
  "B02001_007E") # Estimate!!Total!!Some other race alone

buncombe_data <- get_acs(state = "NC", county = "Buncombe",
                         geography = "block group", variables = acs_vars,
                         geometry = TRUE, cb = FALSE, output = "wide")

buncombe_data <- buncombe_data %>%
  rename(total = B02001_001E,
         white = B02001_002E,
         black = B02001_003E,
         native = B02001_004E,
         asian = B02001_005E) %>%
  mutate(other = total - (white + black + native + asian))

# Total population
buncombe_data %>%
  ggplot(aes(fill = total)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma")

# Proportion white
buncombe_data %>%
  ggplot(aes(fill = white / total)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma")

# Proportion black
buncombe_data %>%
  ggplot(aes(fill = black / total)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma")

##------------
# Combine Data
##------------

# Match up crs
buncombe_data <- st_transform(buncombe_data, crs = st_crs(law_beats))

ggplot() +
  geom_sf(data = buncombe_data, col = "red", fill = NA) +
  geom_sf(data = law_beats, col = "blue", fill = NA)

intersections <- st_intersects(buncombe_data, law_beats)
buncombe_data$intersects <- lengths(intersections) != 0

ggplot() +
  geom_sf(data = buncombe_data, col = "red", fill = NA) +
  geom_sf(data = buncombe_data[buncombe_data$intersects == TRUE, ],
          col = "purple", fill = NA) +
  geom_sf(data = law_beats, col = "blue", fill = NA)

# Summarize
buncombe_summary <- buncombe_data %>%
  st_set_geometry(NULL) %>%
  group_by(intersects) %>%
  summarise(total = sum(total),
         white = sum(white),
         black = sum(black),
         native = sum(native),
         asian = sum(asian),
         other = sum(other))

# Reshape
buncombe_summary <- buncombe_summary %>%
  filter(intersects == TRUE) %>%
  select(-intersects, -total) %>%
  pivot_longer(where(is.numeric),
               names_to = "race") %>%
  mutate(prop = value / sum(value)) %>%
  rename(count = value)

# Clean
data_summary <- data %>%
  mutate(race = if_else(subject_race == "U", "O", subject_race)) %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count)) %>%
  mutate(race = case_when(race == "A" ~ "asian",
                          race == "B" ~ "black",
                          race == "I" ~ "native",
                          race == "O" ~ "other",
                          race == "W" ~ "white"))

# Setup up separation variable
buncombe_summary$data_set <- "buncombe"
data_summary$data_set <- "arrest"
  
# Combine
combine_summary <- rbind(buncombe_summary, data_summary)

ggplot(data = combine_summary,
       aes(reorder(race, -prop), prop, fill = data_set)) +
  scale_y_continuous("Proportion",
                     limits = c(0, 1)) +
  scale_x_discrete("Race",
                   breaks = c("asian",
                              "black",
                              "native",
                              "other",
                              "white"),
                   labels = c("Asian",
                              "Black",
                              "Native American",
                              "Other",
                              "White")) +
  scale_fill_manual("",
                    breaks = c("arrest",
                               "buncombe"),
                    values = c("arrest" = "#760032", # AVL flag
                               "buncombe" = "#03529b"), # AVL symbol
                    labels = c("Arrests",
                               "Asheville Population")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Arrests and Population by Race",
       caption = "Disclaimer: Hispanic is a different feature.") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray75"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "gray85"),
        plot.caption = element_text(hjust = 1))
