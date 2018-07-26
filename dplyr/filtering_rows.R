library(tidyverse)

# Filtering ---------------------------------------------------------------
### basics
msleep %>%
  select(name, sleep_total) %>%
  filter(sleep_total > 18)

### between
msleep %>%
  select(name, sleep_total) %>%
  filter(between(sleep_total, 16, 18))

### near
msleep %>%
  select(name, sleep_total) %>%
  filter(near(sleep_total, 17, tol = sd(sleep_total)))

### condition
msleep %>%
  select(order, name, sleep_total) %>%
  filter(order == "Didelphimorphia")

### %in%
msleep %>%
  select(order, name, sleep_total) %>%
  filter(order %in% c("Didelphimorphia", "Diprotodontia"))

### using pre-defined vectors
remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>%
  select(order, name, sleep_total) %>%
  filter(!order %in% remove)

###
msleep %>%
  select(name, sleep_total) %>%
  filter(str_detect(tolower(name), pattern = "mouse"))

### using multiple predicates
msleep %>%
  select(name, order, sleep_total:bodywt) %>%
  filter(bodywt > 100, (sleep_total > 15 | order != "Carnivora"))

### any_vars is or
msleep %>%
  select(name:order, sleep_total, -vore) %>%
  filter_all(any_vars(str_detect(., pattern = "Ca")))

###
msleep %>%
  select(name, sleep_total:bodywt) %>%
  filter_all(any_vars(. < 0.1))

### all_vars is and
msleep %>%
  select(name, sleep_total:bodywt, -awake) %>%
  filter_all(all_vars(. > 1))

###
msleep %>%
  select(name:order, sleep_total:sleep_rem) %>%
  filter_if(is.character, any_vars(is.na(.)))

### scoped + filtering
msleep %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(sleep_total, sleep_rem), all_vars(.>5))

msleep %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter(sleep_total > 5, sleep_rem > 5)

###
msleep %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(contains("sleep")), all_vars(.>5))
