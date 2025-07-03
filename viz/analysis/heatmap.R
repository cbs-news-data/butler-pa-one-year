# Libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(forcats)
library(makeR)
library(calendR)


# Load your data
by_date <- read.csv("data/visits_by_date.csv")

# Parse date
by_date <- by_date %>%
  mutate(the_date = mdy(the_date))

# Create a complete calendar from min to max date
calendar_data <- tibble(the_date = seq(
  from = floor_date(min(by_date$the_date), "month"),
  to = ceiling_date(max(by_date$the_date), "month") - days(1),
  by = "day"
)) %>%
  left_join(by_date, by = "the_date") %>%
  mutate(
    number_of_visits = replace_na(number_of_visits, 0),
    year = year(the_date),
    month = month(the_date, label = TRUE, abbr = FALSE),
    day = day(the_date),
    weekday = wday(the_date, week_start = 1),   # 1 = Monday
    week = week(the_date),
    fill_group = ifelse(number_of_visits == 0, NA, number_of_visits),  # NA for gray
    week_of_month = 1 + as.integer(difftime(the_date, floor_date(the_date, "month"), units = "weeks"))
  )

calendar_year <- 2023

# Get full sequence of dates for the year
all_dates <- seq.Date(from = as.Date(paste0(calendar_year, "-01-01")),
                      to   = as.Date(paste0(calendar_year, "-12-31")),
                      by = "day")

# Create vector of visit counts in the same order
heatmap_data <- tibble(the_date = all_dates) %>%
  left_join(calendar_data %>% select(the_date, number_of_visits), by = "the_date") %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0))

# Now this vector matches the required input: one number per day of the year
fill_values <- heatmap_data$number_of_visits

# Plot the calendar heatmap
calendR(
  year = calendar_year,
  special.days = fill_values,
  gradient = TRUE,
  low.col= "#fff",
  special.col = "#a5091e",
  title = paste("Visits Heatmap -", calendar_year),
  start = "M",
  legend.pos = "right",
)


