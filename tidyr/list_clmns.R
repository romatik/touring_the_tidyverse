library(dplyr)
library(tidyr)

as_tibble(iris) %>%
  nest(-Species) %>%
  mutate(n = purrr::map_int(data, nrow))

as_tibble(chickwts) %>% nest(weight)


df <- tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h")
)
df %>%
  mutate(y = strsplit(y, ",")) %>%
  unnest(y)

# It also works if you have a column that contains other data frames!
df <- tibble(
  x = 1:2,
  y = list(
    tibble(z = 1),
    tibble(z = 3:4)
  )
)
df %>% unnest(y)

# You can also unnest multiple columns simultaneously
df <- tibble(
  a = list(c("a", "b"), "c"),
  b = list(1:2, 3),
  c = c(11, 22)
)
df %>% unnest(a, b)
# If you omit the column names, it'll unnest all list-cols
df %>% unnest()

# You can also choose to preserve one or more list-cols
df %>% unnest(a, .preserve = b)
