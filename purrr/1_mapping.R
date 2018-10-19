library(purrr)
library(repurrrsive)
suppressPackageStartupMessages(tidyverse)

# map ---------------------------------------------------------------------
# basic usage of map
1:10 %>%
  map(rnorm, n = 10)

1:10 %>%
  map(rnorm, n = 10)

1:10 %>%
  map(function(x) rnorm(10, x))

1:10 %>%
  map(~ rnorm(10, .x))

ex <- list("a" = 3, "b" = 4)

map_at(ex, c("a"), ~.^2)
map_at(ex, 1, ~.^2)

map_if(ex, function(elm) elm == 3, ~.^2)


# got_chars ---------------------------------------------------------------
got_chars <- got_chars %>%
  set_names(map_chr(got_chars, "name"))

# extracting from the list
got_chars %>%
  map("name")

## first element of each list
got_chars %>%
  map(1)

## second book of each list
got_chars %>%
  map(list("books", 2))

# map2 --------------------------------------------------------------------
# from https://jennybc.github.io/purrr-tutorial/ls03_map-function-syntax.html
nms <- got_chars %>%
  map_chr("name")
birth <- got_chars %>%
  map_chr("born")
my_fun <- function(x, y) glue::glue("{x} was born {y}")
map2_chr(nms, birth, my_fun) %>% head()


# pmap --------------------------------------------------------------------
df <- got_chars %>% {
  tibble::tibble(
    name = map_chr(., "name"),
    aliases = map(., "aliases"),
    allegiances = map(., "allegiances")
  )
}
my_fun <- function(name, aliases, allegiances) {
  paste(name, "has", length(aliases), "aliases and",
        length(allegiances), "allegiances")
}
df %>%
  pmap_chr(my_fun) %>%
  tail()

## dealing with more input
df <- got_chars %>% {
  tibble::tibble(
    name = map_chr(., "name"),
    aliases = map(., "aliases"),
    allegiances = map(., "allegiances"),
    url = map_chr(., "url")
  )
}
my_fun <- function(name, aliases, allegiances) {
  paste(name, "has", length(aliases), "aliases and",
        length(allegiances), "allegiances")
}
## fails
df %>%
  pmap_chr(my_fun) %>%
  tail()

## succeeds
my_fun <- function(name, aliases, allegiances, ...) {
  paste(name, "has", length(aliases), "aliases and",
        length(allegiances), "allegiances")
}
df %>%
  pmap_chr(my_fun) %>%
  tail()


# walk --------------------------------------------------------------------
1:10 %>%
  walk(~cat("hello"))


# imap --------------------------------------------------------------------
imap_chr(sample(10), ~ paste0(.y, ": ", .x))

## .x corresponds to value, .y to name/index
my_fun <- function(x, y) glue::glue("{x} was born {y}")
imap_chr(map_chr(got_chars, "born"), my_fun)



# invoke ------------------------------------------------------------------
invoke(runif, list(n = 10))
invoke(runif, n = 10)

invoke_map(list(runif, rnorm), list(list(n = 10), list(n = 5)))
invoke_map(list(runif, rnorm), n = 5)

df <- tibble::tibble(
  f = c("runif", "rpois", "rnorm"),
  params = list(
    list(n = 10),
    list(n = 5, lambda = 10),
    list(n = 10, mean = -3, sd = 10)
  )
)
df
invoke_map(df$f, df$params)


# lmap --------------------------------------------------------------------
# https://d33wubrfki0l68.cloudfront.net/1f648d451974f0ed313347b78ba653891cf59b21/8185b/diagrams/subsetting/train.png
# https://d33wubrfki0l68.cloudfront.net/aea9600956ff6fbbc29d8bd49124cca46c5cb95c/28eaa/diagrams/subsetting/train-single.png

maybe_rep <- function(x) {
  n <- rpois(1, 2)
  out <- rep_len(x, n)
  if (length(out) > 0) {
    names(out) <- paste0(names(x), seq_len(n))
  }
  out
}

# The output size varies each time we map f()
x <- list(a = 1:4, b = letters[5:7], c = 8:9, d = letters[10])
x %>% lmap(maybe_rep)

# We can apply f() on a selected subset of x
x %>% lmap_at(c("a", "d"), maybe_rep)

# Or only where a condition is satisfied
x %>% lmap_if(is.character, maybe_rep)


# dataframing -------------------------------------------------------------
fields <- got_chars %>%
  map(names) %>%
  flatten_chr() %>%
  unique()

library(magrittr)
map_dfr(got_chars, extract, c("name", "culture", "gender", "id", "born", "alive"))

map_dfr(got_chars, extract, fields)

got_chars_tbl <- map_dfc(fields, function(field){
  tibble::tibble(!!field := purrr::map(got_chars, field))
})

got_chars %>% {
  tibble(
    name = map_chr(., "name"),
    culture = map_chr(., "culture"),
    gender = map_chr(., "gender"),
    id = map_int(., "id"),
    born = map_chr(., "born"),
    alive = map_lgl(., "alive")
  )
}
