##---------
# Libraries
##---------
library(tidyverse)
library(here)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(colorspace)
library(ggrepel)

##-----------------------
# Read in data and set up
##-----------------------
data <- read_csv(file = file.path(here::here(), "APD_Arrests.csv"))
head(data)

data_summary <- data %>%
  group_by(date_arrested) %>%
  summarise(total_arrests = n())
head(data_summary)

y_lim_max <- max(data_summary$total_arrests) + 10

##------
# Graphs
##------

# Basic over time ----
ggplot(data = data_summary,
       aes(x = date_arrested,
           y = total_arrests)) +
  geom_line() +
  ylim(c(0, y_lim_max)) +
  labs(x = "Date Arrested",
       y = "Number of Arrests",
       title = "Arrests By Date")
ggsave("Arrests by Date.png")

# Color each year ----
data_summary <- data_summary %>%
  mutate(year_arrested = year(date_arrested),
         month_arrested = month(date_arrested),
         day_arrested = day(date_arrested),
         graph_date = ymd(format(date_arrested, "2020-%m-%d"))) # 2020 leap year

ggplot(data = data_summary,
       aes(x = graph_date,
           y = total_arrests,
           col = as.character(year_arrested))) +
  geom_line() +
  ylim(c(0, y_lim_max)) +
  scale_color_manual(values = sequential_hcl(9, palette = "Viridis")) +
  scale_x_date(date_labels = "%B",
               breaks = as.Date(c("2020-01-01",
                                  "2020-04-01",
                                  "2020-07-01",
                                  "2020-10-01")),
               date_minor_breaks = "1 month") +
  labs(x = "Date Arrested",
       y = "Number of Arrests",
       title = "Arrests By Date",
       col = "Year") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 1))
ggsave("Arrests by Date Color Year.png")

# 7 Day, 30 Day, and Month Average --
data_summary <- data %>%
  group_by(date_arrested) %>%
  summarise(total_arrests = n()) %>%
  arrange(date_arrested) %>%
  mutate(total_arrest_week_avg =
           rollmean(total_arrests, k = 7, fill = NA),
         total_arrest_month_avg =
           rollmean(total_arrests, k = 30, fill = NA)) %>%
  mutate(year_arrested = year(date_arrested),
         month_arrested = month(date_arrested),
         day_arrested = day(date_arrested),
         graph_date = ymd(format(date_arrested, "2020-%m-%d")),
         month_year = ymd(format(date_arrested, "%y-%m-01"))) %>%
  ungroup() %>%
  group_by(year_arrested) %>%
  mutate(total_arrest_cumulative = cumsum(total_arrests)) %>%
  ungroup() %>%
  group_by(month_year) %>%
  mutate(month_average = mean(total_arrests))
  
ggplot(data = data_summary,
       aes(x = graph_date,
           y = total_arrest_week_avg,
           col = as.character(year_arrested))) +
  geom_line() +
  ylim(c(0, y_lim_max)) +
  scale_color_manual(values = sequential_hcl(9, palette = "Viridis")) +
  scale_x_date(date_labels = "%B",
               breaks = as.Date(c("2020-01-01",
                                  "2020-04-01",
                                  "2020-07-01",
                                  "2020-10-01")),
               date_minor_breaks = "1 month") +
  labs(x = "Date Arrested",
       y = "7 Day Average Arrests",
       title = "7 Day Average By Date",
       col = "Year") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 1))
ggsave("Arrests by Date 7 Day Average.png")

ggplot(data = data_summary,
       aes(x = graph_date,
           y = total_arrest_month_avg,
           col = as.character(year_arrested))) +
  geom_line() +
  ylim(c(0, y_lim_max)) +
  scale_color_manual(values = sequential_hcl(9, palette = "Viridis")) +
  scale_x_date(date_labels = "%B",
               breaks = as.Date(c("2020-01-01",
                                  "2020-04-01",
                                  "2020-07-01",
                                  "2020-10-01")),
               date_minor_breaks = "1 month") +
  labs(x = "Date Arrested",
       y = "30 Day Average Arrests",
       title = "30 Day Average By Date",
       col = "Year") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 1))
ggsave("Arrests by Date 30 Day Average.png")

ggplot(data = data_summary,
       aes(x = graph_date,
           y = month_average,
           col = as.character(year_arrested))) +
  geom_line() +
  ylim(c(0, y_lim_max)) +
  scale_color_manual(values = sequential_hcl(9, palette = "Viridis")) +
  scale_x_date(date_labels = "%B",
               breaks = as.Date(c("2020-01-01",
                                  "2020-04-01",
                                  "2020-07-01",
                                  "2020-10-01")),
               date_minor_breaks = "1 month") +
  labs(x = "Date Arrested",
       y = "Average Arrests by Month",
       title = "Month Average By Date",
       col = "Year") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 1))
ggsave("Arrests by Date Month Average.png")

# Cumulative Arrests ----
label_data <- data_summary %>%
  group_by(year_arrested) %>%
  slice_max(date_arrested)

ggplot(data = data_summary,
       aes(x = graph_date,
           y = total_arrest_cumulative,
           col = as.character(year_arrested))) +
  geom_line() +
  scale_color_manual(values = sequential_hcl(9, palette = "Viridis")) +
  scale_y_continuous(label = comma) +
  scale_x_date(date_labels = "%B",
               breaks = as.Date(c("2020-01-01",
                                  "2020-04-01",
                                  "2020-07-01",
                                  "2020-10-01")),
               date_minor_breaks = "1 month",
               limits = as.Date(c("2020-01-01", "2021-01-15"))) +
  labs(x = "Date Arrested",
       y = "Cumulative Arrests",
       title = "Cumulative Arrests By Date") +
  geom_text_repel(data = label_data,
                  aes(x = graph_date,
                      y = total_arrest_cumulative,
                      label = year_arrested),
                  nudge_x      = 15,
                  direction    = "y",
                  hjust        = 0.5,
                  segment.size = 0.2) +
  theme(legend.position = "none")
ggsave("Arrests by Date Cumulative.png")
# Can really see 2020 rise and drop

# Count of arrests per day ----
ggplot(data = data_summary,
       aes(x = total_arrests,
           fill = as.character(year_arrested))) +
  geom_histogram(binwidth = 5,
                 boundary = 0,
                 col = "white") +
  scale_fill_manual(values = sequential_hcl(9, palette = "Viridis")) +
  scale_x_continuous(breaks = NULL,
                     minor_breaks = seq(0, y_lim_max, by = 25)) +
  labs(x = "Number of Arrests",
       y = "Number of Days",
       title = "Number of Days by Number of Arrests",
       fill = "Year") +
  facet_wrap(~ year_arrested, nrow = 3) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))
ggsave("Arrests Count Days.png")
