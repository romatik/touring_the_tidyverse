library(tsibble)
library(lubridate)
weather <- nycflights13::weather %>%
  dplyr::select(origin, time_hour, temp, humid, precip)

weather_tsbl <- tsibble::as_tsibble(weather, key = origin) # interval is automatic
weather_tsbl

weather_tsbl %>%
  dplyr::group_by(origin) %>%
  tsibble::index_by(date = lubridate::as_date(time_hour)) %>%  # like group_by
  dplyr::summarise(
    temp_high = max(temp, na.rm = TRUE),
    temp_low = min(temp, na.rm = TRUE)
  )

# implicit missingness
library(tsibble)
pedestrian

tsibble::has_gaps(pedestrian, .full = TRUE)

ped_gaps <- pedestrian %>%
  tsibble::count_gaps(.full = TRUE)
ped_gaps

ped_full <- pedestrian %>%
  tsibble::fill_gaps(.full = TRUE)
ped_full

pedestrian %>%
  tsibble::fill_gaps(Count = 0L, .full = TRUE)
pedestrian %>%
  dplyr::group_by(Sensor) %>%
  tsibble::fill_gaps(Count = mean(Count), .full = TRUE)

# window functions
# slide()/slide2()/pslide(): sliding window with overlapping observations.
# tile()/tile2()/ptile(): tiling window without overlapping observations.
# stretch()/stretch2()/pstretch(): fixing an initial window and expanding to include more observations.
library(tsibble)
library(dplyr)
pedestrian_full <- pedestrian %>%
  tsibble::fill_gaps(.full = TRUE)
pedestrian_full

pedestrian_full %>%
  dplyr::group_by(Sensor) %>%
  dplyr::mutate(Daily_MA = tsibble::slide_dbl(
                              Count, mean, na.rm = TRUE,
                              .size = 24, .align = "center-left"
  ))

pedestrian_mth <- pedestrian_full %>%
  dplyr::mutate(YrMth = tsibble::yearmonth(Date_Time)) %>% # yearquarter, yearweek, year, extensible
  tidyr::nest(-Sensor, -YrMth)
pedestrian_mth

pedestrian_mth %>%
  dplyr::group_by(Sensor) %>%
  # (1)
  # mutate(Monthly_MA = slide_dbl(data,
  #   ~ mean(bind_rows(.)$Count, na.rm = TRUE), .size = 3, .align = "center"
  # ))
  # (2) equivalent to (1)
  tsibble::mutate(Monthly_MA = tsibble::slide_dbl(data,
                                ~ mean(.$Count, na.rm = TRUE), .size = 3, .align = "center", .bind = TRUE
  ))

# row-oriented workflow
my_diag <- function(...) {
  data <- tibble::tibble(...)
  fit <- lm(Count ~ Time, data = data)
  list(fitted = fitted(fit), resid = residuals(fit))
}

tictoc::tic()
res <- pedestrian %>%
  tsibble::filter_index(~ "2015-03") %>% # until "2015-03", tz aware
  tidyr::nest(-Sensor) %>%
  dplyr::mutate(diag = purrr::map(data, ~ tsibble::pslide_dfr(., my_diag, .size = 24 * 7)))
res
tictoc::toc()

library(furrr)
plan(multiprocess)
tictoc::tic()
res_parallel <- pedestrian %>%
  tsibble::filter_index(~ "2015-03") %>%
  tidyr::nest(-Sensor) %>%
  dplyr::mutate(diag = furrr::future_map(data, ~ tsibble::future_pslide_dfr(., my_diag, .size = 24 * 7)))
tictoc::toc()
