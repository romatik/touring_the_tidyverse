library(tidyverse)

#built-in R dataset
glimpse(msleep)


# Selecting ---------------------------------------------------------------
### basics
msleep %>%
  select(name, genus, sleep_total, awake) %>%
  glimpse()

### selecting with `:`
msleep %>%
  select(name:order, sleep_total:sleep_cycle) %>%
  glimpse

### dropping columns
msleep %>%
  select(-conservation, -(sleep_total:awake)) %>%
  glimpse

###
# `conservation` is in the range name:awake
msleep %>%
  select(-(name:awake), conservation) %>%
  glimpse

### using character vectors to subset
classification_info <- c("name", "genus", "vore", "order", "conservation")
sleep_cols <- c("sleep_total", "sleep_rem", "sleep_cycle")
weight_cols <- c("brainwt", "bodywt")

msleep %>%
  select(one_of(sleep_cols), one_of(weight_cols))

### using tidyselect helpers
msleep %>%
  select(name, starts_with("sleep")) %>%
  glimpse


###
msleep %>%
  select(contains("eep"), ends_with("wt")) %>%
  glimpse

###
msleep %>%
  select(matches("o.+er")) %>%
  glimpse


### using predicates
msleep %>%
  select_if(is.numeric) %>%
  glimpse

###
msleep %>%
  select_if(~!is.numeric(.)) %>%
  glimpse

msleep %>%
  select_if(funs(!is.numeric(.))) %>%
  glimpse


### multiple predicates
msleep %>%
  select_if(~is.numeric(.),  ~mean(., na.rm=TRUE) > 10) %>%
  glimpse()

###
msleep %>%
  select_if(~n_distinct(.) < 10)


# Re-ordering -------------------------------------------------------------
msleep %>%
  select(conservation, sleep_total, name) %>%
  glimpse


###
msleep %>%
  select(conservation, sleep_total, everything()) %>%
  glimpse


# Column names ----------------------------------------------------------------
###
msleep %>%
  select(animal = name, sleep_total,
         extinction_threat = conservation) %>%
  glimpse

msleep %>%
  rename(animal = name, extinction_threat = conservation) %>%
  glimpse

###
msleep %>%
  select_all(toupper)

select_all(mtcars, "toupper")
select_all(mtcars, funs(toupper(.)))

###
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("name", "sleep total", "brain weight")

msleep2 %>%
  select_all(~str_replace(., " ", "_"))

###
mtcars %>%
  head

mtcars %>%
  rownames_to_column("car_model") %>%
  head


### pulling column out
mtcars %>%
  pull(cyl)

### extracting columns
msleep %>%
  first()

msleep %>%
  `[[`(1)

msleep %>%
  last()

msleep %>%
  nth(2)

