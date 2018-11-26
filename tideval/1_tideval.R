library(magrittr)
library(rlang)

# Motivation --------------------------------------------------------------
## duplicated code
(df$a - min(df$a)) / (max(df$a) - min(df$a))
(df$b - min(df$b)) / (max(df$b) - min(df$b))
(df$c - min(df$c)) / (max(df$c) - min(df$c))
(df$d - min(df$d)) / (max(df$d) - min(df$c))

## abstraction step to emphasize sameness
(num - min(num)) / (max(num) - min(num))

rescale01 <- function(num) {
  (num - min(num)) / (max(num) - min(num))
}

## final solution
rescale01 <- function(num) {
  rng <- range(num, na.rm = TRUE, finite = TRUE)
  (num - rng[[1]]) / (rng[[2]] - rng[[1]])
}

rescale01(df$a)
rescale01(df$b)
rescale01(df$c)
rescale01(df$d)

## now let's try with dplyr
df1 %>% group_by(x1) %>% summarise(mean = mean(y1))
df2 %>% group_by(x2) %>% summarise(mean = mean(y2))
df3 %>% group_by(x3) %>% summarise(mean = mean(y3))
df4 %>% group_by(x4) %>% summarise(mean = mean(y4))

## abstraction step

## "final" solution


# Evaluating vs quoting functions -----------------------------------------
## Evaluating
identity(6)

identity(2 * 3)

a <- 2
b <- 3
identity(a * b)

## Quoting
quote(6)

quote(2 * 3)

quote(a * b)

## `$` vs `[[`
df <- data.frame(
  y = 1,
  var = 2
)

 # Indirect
 # Direct


 # Direct
 # Direct

# Unquoting ---------------------------------------------------------------
x_var <- quote(cyl)
y_var <- quote(mpg)

x_var
y_var

library("dplyr")
by_cyl <- mtcars %>%
  group_by(!!x_var) %>%            # Refer to x_var
  summarise(mean = mean(!!y_var))  # Refer to y_var

library("ggplot2")
ggplot(mtcars, aes(!!x_var, !!y_var)) +  # Refer to x_var and y_var
  geom_point()

ggplot(mtcars, aes(disp, drat)) +
  geom_point() +
  facet_grid(vars(!!x_var))  # Refer to x_var

## lobstr
library(lobstr)
a <- 2

## R code is a tree

## You can capture the tree by quoting

## Unquoting makes it easy to build trees

## `qq_show()`
rlang::qq_show(mtcars %>% group_by(!!x_var))
rlang::qq_show(data %>% summarise(mean = mean(!!y_var)))
rlang::qq_show(ggplot(mtcars, aes(!!x_var, !!y_var)))
rlang::qq_show(facet_grid(vars(!!x_var)))

# Quoting + unquoting to write functions. ---------------------------------
## abstraction step
grouped_mean <- function(data, group_var, summary_var) {
  data %>%
    group_by(group_var) %>%
    summarise(mean = mean(summary_var))
}

## quoting

## unquoting

## result

# Multiple arguments ------------------------------------------------------
## working with dots

### evaluate

materialise(mtcars, 1 + 2, important_name = letters)

### quote

capture(mtcars, 1 + 2, important_name = letters)

### forward
forward <- function(data, ...) {
  forwardee(...)
}
forwardee <- function(foo, bar, ...) {
  list(foo = foo, bar = bar, ...)
}
forward(mtcars, bar = 100, 1, 2, 3)

## quoting multiple arguments with tidyeval

vars <- list(
  quote(cyl),
  quote(am)
)
rlang::qq_show(group_by(!!vars))

rlang::qq_show(group_by(!!!vars))

# Modifying names ---------------------------------------------------------
##
grouped_mean2 <- function(.data, .summary_var, ...) {
  summary_var <- enquo(.summary_var)

  # Quote the dots with default names
  group_vars <- enquos(..., .named = TRUE)

  summary_nm <- quo_name(summary_var)
  summary_nm <- paste0("avg_", summary_nm)

  # Modify the names of the list of quoted dots
  names(group_vars) <- paste0("groups_", names(group_vars))

  .data %>%
    group_by(!!!group_vars) %>%  # Unquote-splice as usual
    summarise(!!summary_nm := mean(!!summary_var))
}

grouped_mean2(mtcars, disp, cyl, am)

##
unselect <- function(.data, ...){
  dots <- rlang::enquos(...) %>%
    purrr::map(function(variable){
      rlang::expr(-!!variable)
    })
  .data %>%
    dplyr::select(!!!dots)
}

unselect(tibble::as_tibble(mtcars), mpg:disp, qsec)

# Quosures capture expression and environment.
my_mutate <- function(df, var){
  n <- 10
  var <- rlang::enexpr(var)
  mutate(df, !!var)
}

df <- tibble(x = 1)
n <- 100
my_mutate(df, x + n)

my_mutate2 <- function(df, var){
  n <- 10
  var <- rlang::enquo(var)
  mutate(df, !!var)
}

df <- tibble(x = 1)
n <- 100
my_mutate2(df, x + n)
