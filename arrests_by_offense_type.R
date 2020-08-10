##---------
# Libraries
##---------
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)
library(rvest)
library(treemap)
library(colorspace)

##------------
# Read in data
##------------
data <- read_csv(file = file.path(here::here(), "APD_Arrests.csv"))
head(data)

##-----------------
# Get offense types
##-----------------

# https://www.nccourts.gov/documents/publications
## /nc-courts-offense-codes-and-classes
offense_codes <- readxl::read_xls(here::here("offense_codes_and_classes_1.xls"),
                                  sheet = "AOC ACIS Codes")
####----
# Notes:
# D : "*" means not in use and "I" means replaced by an Infraction
# CODE : Offense Code will be repeated when changes in Class occure
# T : T=Traffic, and I=Infraction
#   : blank=Clerk has to entered F, M, T, or I
# Offense Description :
# NC General Statute :
# Offense State Date : Date Offense Code Applies
# Offense End Date : Date Offense Code Applies
# CL : Structured Sentencing Class
#    : ?? Means Mixed Classes
# Class Start Date : Date Class Code Applies
# Class End Date : Date Class Code Applies
# DWI and IC Report :
# Max Date : Maximum of Start and End Offense Code Dates and Class dates
####----
head(offense_codes)

names(offense_codes) <- c("D", "code", "T", "description", "statute",
                          "offense_start_date", "offense_end_date",
                          "class", "class_start_date", "class_end_date",
                          "dwi_ic_report", "max_date")

# Clean up data ----
offense_codes <- offense_codes %>%
  mutate(offense_start_date = as.Date(offense_start_date, format = "%Y%m%d"),
         offense_end_date = as.Date(offense_end_date, format = "%Y%m%d"),
         class_start_date = as.Date(class_start_date, format = "%Y%m%d"),
         class_end_date = as.Date(class_end_date, format = "%Y%m%d")) %>%
  mutate(offense_start_date = if_else(is.na(offense_start_date),
                                      as.Date("1900-01-01"),
                                      offense_start_date),
         offense_end_date = if_else(is.na(offense_end_date),
                                    as.Date("9999-01-01"),
                                    offense_end_date),
         class_start_date = if_else(is.na(class_start_date),
                                    as.Date("1900-01-01"),
                                    class_start_date),
         class_end_date = if_else(is.na(class_end_date),
                                  as.Date("9999-01-01"),
                                  class_end_date))

## Code to save missing types ----
# distinct_offense_codes <- offense_codes %>%
#   select("description") %>%
#   distinct() %>%
#   pull()
#
# distinct_offense_type <- data %>%
#   select("offense_type") %>%
#   distinct() %>%
#   pull()
#
# missing_types <- distinct_offense_type[!(distinct_offense_type %in%
#                                           distinct_offense_codes)]
# write_csv(data.frame(offense_type = missing_types),
#           path = here::here("missing_types.csv"))

## Do some individual cleaning ----

## Cleaned up Offense Codes ----
missing_types <- read_csv(here::here("missing_types.csv"))

##--------------------------
# Join offense types to data
##--------------------------
data <- data %>%
  left_join(missing_types, by = c("offense_type" = "offense_type")) %>%
  mutate(offense_type = ifelse(!is.na(Update), Update, offense_type)) %>%
  select(-Update, -Cleaned)

# Clean up dates ----
data_offense_codes <- data %>%
  left_join(offense_codes, by = c("offense_type" = "description")) %>%
  mutate(offense_start_date = if_else(is.na(offense_start_date),
                                      as.Date("1900-01-01"),
                                      offense_start_date),
         offense_end_date = if_else(is.na(offense_end_date),
                                    as.Date("9999-01-01"),
                                    offense_end_date),
         class_start_date = if_else(is.na(class_start_date),
                                    as.Date("1900-01-01"),
                                    class_start_date),
         class_end_date = if_else(is.na(class_end_date),
                                  as.Date("9999-01-01"),
                                  class_end_date))

# Filter by dates ----
data_dates_in <- data_offense_codes %>%
  filter(date_arrested >= offense_start_date &
           date_arrested <= offense_end_date &
           date_arrested >= class_start_date &
           date_arrested <= class_end_date)

data_missing <- data_offense_codes %>%
  filter(!(data_offense_codes$OBJECTID %in% data_dates_in$OBJECTID))

data_offense_codes <- rbind(data_dates_in, data_missing)

## De-duplicate
duplicates <- data_offense_codes %>%
  group_by(OBJECTID) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  select(-count)

# Take highest code?
duplicates <- duplicates %>%
  group_by(OBJECTID) %>%
  slice_max(1, code, class_start_date)

data_offense_codes <- data_offense_codes %>%
  filter(!(data_offense_codes$OBJECTID %in% duplicates$OBJECTID))

data_offense_codes <- rbind(data_offense_codes, duplicates)
# Now original data and offense match up in number of arrest

##--------------------
# Get statute chapters
##--------------------
statutes_web_page <- read_html("https://www.ncleg.gov/Laws/GeneralStatutesTOC")
statutes_web_page <- statutes_web_page %>%
  html_nodes("a") %>%
  html_text()

statutes_web_page <- as.data.frame(statutes_web_page)

statutes_web_page <- statutes_web_page %>%
  mutate(is_title = if_else(substr(statutes_web_page, 1, 7) == "Chapter",
                            1, 0)) %>%
  mutate(is_chapter = lag(is_title),
         title = lag(statutes_web_page)) %>%
  filter(is_chapter == 1) %>%
  select(title, statutes_web_page) %>%
  rename(chapter = statutes_web_page) %>%
  mutate(chapter_number = sub(".*\\s", "", title))

# Join statute chapters to data ----
data_offense_codes <- data_offense_codes %>%
  mutate(statute_chapter = sub("(^[^-]+)-.*", "\\1", statute))

data_offense_codes <- data_offense_codes %>%
  left_join(statutes_web_page, by = c("statute_chapter" = "chapter_number"))

data_offense_codes <- data_offense_codes %>%
  select(OBJECTID,
         offense_type,
         code,
         statute,
         class,
         statute_chapter,
         chapter) %>%
  mutate(statute = if_else(is.na(statute), offense_type, statute),
         chapter = if_else(is.na(chapter), statute, chapter))

##----------
# Get counts
##----------
data_summary <- data_offense_codes %>%
  group_by(chapter) %>%
  summarise(count = n())

# Combine small statute counts into "OTHER"
data_summary <- data_offense_codes %>%
  select(chapter, statute) %>%
  mutate(total_count = n()) %>%
  group_by(chapter) %>%
  mutate(chapter_count = n()) %>%
  mutate(chapter_prop = chapter_count / total_count) %>%
  mutate(chapter = if_else(chapter_prop < .01, "OTHER", chapter),
         statute = if_else(chapter_prop < .01, "OTHER", statute)) %>%
  group_by(chapter) %>%
  mutate(chapter_count = n()) %>%
  group_by(chapter, chapter_count, statute) %>%
  summarise(statute_count = n()) %>%
  mutate(statute_chapter_prop = statute_count / chapter_count) %>%
  mutate(statute = if_else(statute_chapter_prop < .0075, "OTHER", statute)) %>%
  group_by(chapter, chapter_count, statute) %>%
  summarise(count = sum(statute_count))

data_summary <- data_summary %>%
  mutate(chapter = paste0(chapter, " (",
                          format(chapter_count,
                                 big.mark = ",",
                                 trim = TRUE), ")"))

count_chapters <- length(unique(data_summary$chapter))

##------
# Graphs
##------
png(filename = here::here("offense_codes.png"),
    width = 800,
    height = 800)
treemap(data_summary,
        index = c("chapter", "statute"),
        vSize = "count",
        type = "index",
        fontcolor.labels = c("white", "black"),
        fontface.labels = c(2, 1),
        bg.labels = c("transparent"),
        border.col = c("black", "gray"),
        border.lwds = c(5, 2),
        title = "",
        palette =
          qualitative_hcl(count_chapters, palette = "Dark3")[count_chapters:1]
        )
dev.off()

## Same analysis, but only Criminal Law
data_summary <- data_offense_codes %>%
  filter(chapter == "Criminal Law")

data_summary <- data_summary %>%
  select(statute, offense_type) %>%
  mutate(total_count = n()) %>%
  group_by(statute) %>%
  mutate(statute_count = n()) %>%
  mutate(statute_prop = statute_count / total_count) %>%
  mutate(statute = if_else(statute_prop < .01, "OTHER", statute),
         offense_type = if_else(statute_prop < .01, "OTHER", offense_type)) %>%
  group_by(statute) %>%
  mutate(statute_count = n()) %>%
  group_by(statute, statute_count, offense_type) %>%
  summarise(offense_type_count = n()) %>%
  mutate(offense_type_statute_prop = offense_type_count / statute_count) %>%
  mutate(offense_type = if_else(offense_type_statute_prop < .0075,
                                "OTHER", offense_type)) %>%
  group_by(statute, statute_count, offense_type) %>%
  summarise(count = sum(offense_type_count))

data_summary <- data_summary %>%
  mutate(statute = paste0(statute, " (",
                          format(statute_count,
                                 big.mark = ",",
                                 trim = TRUE), ")"))

count_statutes <- length(unique(data_summary$statute))

png(filename = here::here("criminal_law_statutes.png"),
    width = 800,
    height = 800)
treemap(data_summary,
        index = c("statute", "offense_type"),
        vSize = "count",
        type = "index",
        fontcolor.labels = c("white", "black"),
        fontface.labels = c(2, 1),
        bg.labels = c("transparent"),
        border.col = c("black", "gray"),
        border.lwds = c(5, 2),
        title = "Criminal Law",
        palette =
          qualitative_hcl(count_chapters, palette = "Dark3"),
        align.labels = list(
          c("center", "center"),
          c("right", "bottom")
        )
)
dev.off()
