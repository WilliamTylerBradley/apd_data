library(tidyverse)
library(here)
library(lubridate)

data <- read_csv(file = file.path(here::here(), "APD_CAD_911_Calls.csv"))

data <- data %>%
  mutate(am_pm = lubridate::am(call_time),
         date = as.Date(call_time))

distinct_data <- data %>%
  select(address, call_nature, call_disposition, am_pm, date) %>%
  group_by(address, call_nature, call_disposition, am_pm, date) %>%
  summarize(count = n())

distinct_address <- data %>%
  distinct(address)


