library(tidyverse)

###
msleep %>%
  count(order, sort = TRUE)

###
msleep %>%
  count(order, vore, sort = TRUE)

###
msleep %>%
  tally()

###
msleep %>%
  select(1:3) %>%
  add_tally()

###
msleep %>%
  select(name:vore) %>%
  add_count(vore)

###
msleep %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

###
msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

###
msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm = TRUE)

###
msleep %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)

###
msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

###
msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  rename_if(is.numeric, ~paste0("avg_", .))

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, funs(avg = mean(.)), na.rm = TRUE)

###
msleep %>%
  group_by(vore) %>%
  summarise_at(vars(contains("sleep")), mean, na.rm=TRUE) %>%
  rename_at(vars(contains("sleep")), ~paste0("avg_", .))

###
msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total)) %>%
  arrange(desc(avg_sleep))

###
msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

###
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

###
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(-5)

###
msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

###
msleep %>%
  sample_frac(.1)

msleep %>%
  sample_n(10)


###
msleep %>%
  slice(50:55)
