library(dplyr)
library(tidyr)
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% drop_na()
df %>% drop_na(x)
df %>% drop_na(starts_with("x"))

# fills with the previous non-missing value. Useful when only changes are recorded
df %>% fill(Year)

# replacing NA's
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = list(1:5, NULL, 10:20))
df %>% replace_na(list(x = 0, y = "unknown"))
df %>% mutate(x = replace_na(x, 0))
