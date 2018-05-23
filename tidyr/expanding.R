library(dplyr)
library(tidyr)

df <- tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)

df %>% complete(group, nesting(item_id, item_name))
df %>% complete(group, item_id, item_name)

# filling data in
df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0))

# full sequence
full_seq(c(1, 2, 4, 5, 10), 1)

# expanding
expand(mtcars, vs, cyl)
# Only combinations of vs and cyl that appear in the data
expand(mtcars, nesting(vs, cyl))

# implicit missings
df <- tibble(
  year   = c(2010, 2010, 2010, 2010, 2012, 2012, 2012),
  qtr    = c(   1,    2,    3,    4,    1,    2,    3),
  return = rnorm(7)
)
df %>% expand(year, qtr)
df %>% expand(year = 2010:2012, qtr)
df %>% expand(year = full_seq(year, 1), qtr)
df %>% complete(year = full_seq(year, 1), qtr)


# Nesting -------------------------------------------------------------------
# Each person was given one of two treatments, repeated three times
# But some of the replications haven't happened yet, so we have
# incomplete data:
experiment <- tibble(
  name = rep(c("Alex", "Robert", "Sam"), c(3, 2, 1)),
  trt  = rep(c("a", "b", "a"), c(3, 2, 1)),
  rep = c(1, 2, 3, 1, 2, 1),
  measurment_1 = runif(6),
  measurment_2 = runif(6)
)

# We can figure out the complete set of data with expand()
# Each person only gets one treatment, so we nest name and trt together:
all <- experiment %>% expand(nesting(name, trt), rep)
all
all %>% anti_join(experiment)
experiment %>% right_join(all)

experiment %>% complete(nesting(name, trt), rep)
