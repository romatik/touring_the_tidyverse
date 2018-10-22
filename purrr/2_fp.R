library(magrittr)
library(purrr)

# reduce ------------------------------------------------------------------
1:3 %>% reduce(`+`)
1:10 %>% reduce(`*`)

paste2 <- function(x, y, sep = ".") paste(x, y, sep = sep)
letters[1:4] %>% reduce(paste2)
letters[1:4] %>% reduce2(c("-", ".", "-"), paste2)
letters[1:8] %>% reduce2(c("-----", ".", "----",".", "---", ".", "--"), paste2)

# accumulate --------------------------------------------------------------
1:3 %>% accumulate(`+`)
1:10 %>% accumulate_right(`*`)
1:10 %>% accumulate(max, .init = 5)

1:10 %>% accumulate(~ .x)
1:10 %>% accumulate(~ .y)
c(100, 1:9) %>% accumulate(~ 1 + .y)
c(100, 1:9) %>% accumulate(~ 1 + .x)

# For each position of x, I want to count how many numbers were > 5
x<-c(2,8,4,9,10,6,7,3,1,5)
accumulate(x > 5, `+`)

# compose -----------------------------------------------------------------
not_null <- compose(`!`, is.null)
not_null(4)
not_null(NULL)

not_null2 <- negate(is.null)
not_null2(4)
not_null2(NULL)


# partial -----------------------------------------------------------------
l <- list(1:3, NULL, 5:10)

compact1 <- function(x) discard(x, is.null)
compact2 <- partial(discard, .p = is.null)

compact1(l)
compact2(l)

# source https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
library(dplyr)
p_funs <- c(0.2, 0.5, 0.8) %>%
  set_names(map_chr(., ~paste0(.x*100, "%"))) %>%
  map(~partial(quantile, probs = .x, na.rm = TRUE))
p_funs

mtcars %>%
  group_by(cyl) %>%
  summarize_at(vars(mpg), funs(!!!p_funs))

mtcars %>%
  group_by(cyl) %>%
  summarize_at(vars(mpg, hp), funs(!!!p_funs))
