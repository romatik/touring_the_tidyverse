library(dplyr)
library(tidyr)

# From http://stackoverflow.com/questions/1181060
stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)

# works with grouped tibbles
mini_iris <-
  iris %>%
  group_by(Species) %>%
  slice(1)
mini_iris %>% gather(key = flower_att, value = measurement, -Species)

# can use selector from dplyr
iris %>%
  tibble::as.tibble() %>%
  gather(key = dimensions, value = measurement, c(contains(".Length"), contains(".Width")))

# http://stackoverflow.com/questions/15668870/
grades <- tbl_df(read.table(header = TRUE, text = "
   ID   Test Year   Fall Spring Winter
    1   1   2008    15      16      19
    1   1   2009    12      13      27
    1   2   2008    22      22      24
    1   2   2009    10      14      20
    2   1   2008    12      13      25
    2   1   2009    16      14      21
    2   2   2008    13      11      29
    2   2   2009    23      20      26
    3   1   2008    11      12      22
    3   1   2009    13      11      27
    3   2   2008    17      12      23
    3   2   2009    14      9       31
"))

grades %>%
  gather(Semester, Score, Fall:Winter) %>%
  mutate(Test = paste0("Test", Test)) %>%
  spread(Test, Score) %>%
  arrange(ID, Year, Semester)


# http://stackoverflow.com/questions/16032858
results <- data.frame(
  Ind = paste0("Ind", 1:10),
  Treatment = rep(c("Treat", "Cont"), each = 10),
  value = 1:20
)

results %>% spread(Treatment, value)


# Use 'convert = TRUE' to produce variables of mixed type
df <- data.frame(row = rep(c(1, 51), each = 3),
                 var = c("Sepal.Length", "Species", "Species_num"),
                 value = c(5.1, "setosa", 1, 7.0, "versicolor", 2)) %>%
  tibble::as_tibble()
df %>% spread(var, value) %>% str
df %>% spread(var, value, convert = TRUE) %>% str


# http://stackoverflow.com/questions/9684671
set.seed(10)
activities <- tibble::tibble(
  id = sprintf("x1.%02d", 1:10),
  trt = sample(c('cnt', 'tr'), 10, T),
  work.T1 = runif(10),
  play.T1 = runif(10),
  talk.T1 = runif(10),
  work.T2 = runif(10),
  play.T2 = runif(10),
  talk.T2 = runif(10)
)

activities %>%
  gather(key, value, -id, -trt) %>%
  separate(key, into = c("location", "time")) %>%
  arrange(id, trt, time) %>%
  spread(location, value)


# https://www.daeconomist.com/post/2018-05-15-spread/
df <- tibble::tibble(age = c(21, 17, 32, 29, 15),
                     gender = c("Male", "Female", "Female", "Male", "Male")) %>%
  dplyr::mutate(gender = as.factor(gender))

df %>%
  spread(gender, age)

# Expected output
## # A tibble: 3 x 2
##   Female Male
## * <chr>  <chr>
## 1 17     21
## 2 32     29
## 3 <NA>   15

df <- df %>%
  group_by(gender) %>%
  mutate(grouped_id = row_number())
df

df %>%
  spread(gender, age) %>%
  select(-grouped_id)
