library(tsibble)
library(lubridate)
weather <- nycflights13::weather %>%
  select(origin, time_hour, temp, humid, precip)

weather_tsbl <- as_tsibble(weather, key = origin) # interval is automatic
weather_tsbl

weather_tsbl %>%
  group_by(origin) %>%
  index_by(date = as_date(time_hour)) %>%  # like group_by
  summarise(
    temp_high = max(temp, na.rm = TRUE),
    temp_low = min(temp, na.rm = TRUE)
  )

# implicit missingness
library(tsibble)
pedestrian

has_gaps(pedestrian, .full = TRUE)

ped_gaps <- pedestrian %>%
  count_gaps(.full = TRUE)
ped_gaps

ped_full <- pedestrian %>%
  fill_gaps(.full = TRUE)
ped_full

pedestrian %>%
  fill_gaps(Count = 0L, .full = TRUE)
pedestrian %>%
  group_by(Sensor) %>%
  fill_gaps(Count = mean(Count), .full = TRUE)

# window functions
# slide()/slide2()/pslide(): sliding window with overlapping observations.
# tile()/tile2()/ptile(): tiling window without overlapping observations.
# stretch()/stretch2()/pstretch(): fixing an initial window and expanding to include more observations.
library(tsibble)
library(dplyr)
pedestrian_full <- pedestrian %>%
  fill_gaps(.full = TRUE)
pedestrian_full

pedestrian_full %>%
  group_by(Sensor) %>%
  mutate(Daily_MA = slide_dbl(Count,
                              mean, na.rm = TRUE, .size = 24, .align = "center-left"
  ))

pedestrian_mth <- pedestrian_full %>%
  mutate(YrMth = yearmonth(Date_Time)) %>% # yearquarter, yearweek, year, extensible
  nest(-Sensor, -YrMth)
pedestrian_mth

pedestrian_mth %>%
  group_by(Sensor) %>%
  # (1)
  # mutate(Monthly_MA = slide_dbl(data,
  #   ~ mean(bind_rows(.)$Count, na.rm = TRUE), .size = 3, .align = "center"
  # ))
  # (2) equivalent to (1)
  mutate(Monthly_MA = slide_dbl(data,
                                ~ mean(.$Count, na.rm = TRUE), .size = 3, .align = "center", .bind = TRUE
  ))

# row-oriented workflow
my_diag <- function(...) {
  data <- tibble(...)
  fit <- lm(Count ~ Time, data = data)
  list(fitted = fitted(fit), resid = residuals(fit))
}
tictoc::tic()
pedestrian %>%
  filter_index(~ "2015-03") %>% # until "2015-03"
  nest(-Sensor) %>%
  mutate(diag = purrr::map(data, ~ pslide_dfr(., my_diag, .size = 24 * 7)))
tictoc::toc()

library(furrr)
plan(multiprocess)
tictoc::tic()
pedestrian %>%
  filter_index(~ "2015-03") %>%
  nest(-Sensor) %>%
  mutate(diag = future_map(data, ~ future_pslide_dfr(., my_diag, .size = 24 * 7)))
tictoc::toc()

