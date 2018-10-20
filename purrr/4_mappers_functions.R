library(tidyverse)

urls <- list(
  example = "http://example.org",
  asdf = "http://asdfasdasdkfjlda"
)
map(urls, readr::read_lines)


# safely ------------------------------------------------------------------
safe_read_lines <- safely(readr::read_lines)
safe_read_lines
html <- map(urls, safe_read_lines)

not_null <- negate(is.null)
res <- transpose(html)[["result"]] %>% keep(not_null)
errs <- transpose(html)[["error"]] %>% keep(not_null)


# possibly ----------------------------------------------------------------
possibly_read_lines <- possibly(readr::read_lines, otherwise = "No dice")
html <- map(urls, possibly_read_lines)

html[["asdf"]]


# quietly -----------------------------------------------------------------
res <- list("a", 1, 2, 3) %>%
  map(quietly(as.numeric))

str(res)

res %>% map("warnings")

# auto_browse -------------------------------------------------------------
f <- function(x) {
  y <- 20
  if (x > 5) {
    stop("!")
  } else {
    x
  }
}
if (interactive()) {
  map(1:6, auto_browse(f))
}


# lifting -----------------------------------------------------------------
x <- list(x = c(1:100, NA, 1000), na.rm = TRUE, trim = 0.9)
lift_dl(mean)(x)

# Or in a pipe:
mean %>% lift_dl() %>% invoke(x)

l <- list(3, NA, 4, na.rm = TRUE)
fun <- sum %>% lift_dl()
fun(l)
sum(l)


# mappers -----------------------------------------------------------------
as_mapper(~ . + 1)
as_mapper(1)

as_mapper(c("a", "b", "c"))
# Equivalent to function(x) x[["a"]][["b"]][["c"]]

as_mapper(list(1, "a", 2))
# Equivalent to function(x) x[[1]][["a"]][[2]]

as_mapper(list(1, attr_getter("a")))
# Equivalent to function(x) attr(x[[1]], "a")

as_mapper(c("a", "b", "c"), .null = NA)

