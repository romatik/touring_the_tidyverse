library(tidyverse)

# Mutating ----------------------------------------------------------------
### basics
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_min = sleep_total * 60)

msleep %>%
  transmute(sleep_total_min = sleep_total * 60)

### using formulas
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_vs_avg = sleep_total - round(mean(sleep_total), 1),
         sleep_total_vs_min = sleep_total - min(sleep_total))

### multiple ways to vectorize non-vectorized function
msleep %>%
  select(name, contains("sleep")) %>%
  rowwise() %>%
  mutate(avg = mean(c(sleep_rem, sleep_cycle)))

msleep %>%
  select(name, contains("sleep")) %>%
  mutate(avg = purrr::map2_dbl(sleep_rem, sleep_cycle, ~mean(c(.x, .y))))

msleep %>%
  mutate(anotherwt = bodywt) %>%
  mutate(grand_mean = select(., ends_with("wt")) %>%
           purrr::pmap_dbl(~mean(c(...))))

### vectorized ifelse
msleep %>%
  select(name, brainwt) %>%
  mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
  arrange(desc(brainwt))

### scoped mutates
msleep %>%
  mutate_all(tolower)

###
msleep %>%
  select(name, sleep_total:bodywt) %>%
  mutate_if(is.numeric, round)

### scoped + tidyselect
msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60))

###
msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60)) %>%
  rename_at(vars(contains("sleep")), ~paste0(.,"_min"))

msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), funs(min = .*60))


### recoding variables
msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)


###
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr =
    case_when(
      sleep_total > 13 ~ "very long",
      sleep_total > 10 ~ "long",
      sleep_total > 7 ~ "limited",
      TRUE ~ "short")) %>%
  mutate(sleep_total_discr = factor(sleep_total_discr, levels = c("short", "limited", "long", "very long")))

###
msleep %>%
  mutate(silly_groups = case_when(
    brainwt < 0.001 ~ "light_headed",
    sleep_total > 10 ~ "lazy_sleeper",
    is.na(sleep_rem) ~ "absent_rem",
    TRUE ~ "other")) %>%
  count(silly_groups)

### creating NA's
msleep %>%
  select(name:order) %>%
  na_if("omni")

