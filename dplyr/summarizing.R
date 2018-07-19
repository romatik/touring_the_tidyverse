library(tidyverse)

### group_by and count
msleep %>%
  count(order, sort = TRUE)

### group_by multiple columns
msleep %>%
  count(order, vore, sort = TRUE)

### count number of rows
msleep %>%
  tally()

### add tally to all rows
msleep %>%
  select(1:3) %>%
  add_tally()

### add count of specific column
msleep %>%
  select(name:vore) %>%
  add_count(vore)

### summarize with multiple functions
msleep %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

### summarized group_by
msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

### scoped summarize
msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm = TRUE)

### using function
msleep %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)

### predicate
msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

### adding name of the function to the column name
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

### arranging inside of each group
msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

### top 5 by average
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5, wt = average)

### bottom 5 by average
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(-5)

###
msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

### sampling
msleep %>%
  sample_frac(.1)

msleep %>%
  sample_n(10)


### slicing
msleep %>%
  slice(50:55)
