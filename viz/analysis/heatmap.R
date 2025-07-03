# Libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(forcats)
library(makeR)

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

# Plot
ggplot(calendar_data, aes(x = weekday, y = -week_of_month, fill = fill_group)) +
  geom_tile(color = "white") +
  facet_wrap(~year + month, ncol = 4) +
  scale_x_continuous(
    breaks = 1:7,
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_c(
    option = "C",
    name = "Visits",
    na.value = "gray90"  # 0s become gray
  ) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Calendar Heatmap of Daily Visits",
    x = "Day of Week",
    y = "Week of Month"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )



calendarHeat(
  dates = by_date$the_date,
  values = by_date$number_of_visits,
  varname = "Visits"
)

# Also export the makeR calendarHeat to SVG
svg("calendar_heatmap_makeR.svg", width = 10, height = 6)


dev.off()
