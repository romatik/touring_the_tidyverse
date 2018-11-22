library(rlang)
library(lobstr)
library(magrittr)

a <- 2
ast(1 + !!a)
ast(1 + a)
ast(1 + !!a * !!a)
ast(1 + a * a)

a <- expr(b + 1)
ast(1 + !!a * !!a)
ast(1 + a * a)


unselect <- function(.data, ...){
  dots <- rlang::enquos(...) %>%
    purrr::map(function(variable){
      rlang::expr(-!!variable)
    })
  .data %>%
    dplyr::select(!!!dots)
}

unselect(tibble::as_tibble(mtcars), mpg:disp, qsec)


rlang::qq_show(!!!unselect(mpg, cyl))

