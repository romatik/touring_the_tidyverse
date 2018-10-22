library(purrr)

# flattenning -----------------------------------------------------------------
x <- rerun(2, sample(4))
x
x %>% flatten()
x %>% flatten_int()
x %>% map(1L) %>% flatten_int()

as.list(letters) %>% as_vector("character")
as.list(letters) %>% as_vector(character(1))
list(1:2, 3:4, 5:6) %>% as_vector(integer(2))

as.list(letters) %>% simplify()
list(1:2, 3:4, 5:6) %>% simplify_all()


# crossing ----------------------------------------------------------------
data <- list(
  id = c("John", "Jane"),
  greeting = c("Hello.", "Bonjour."),
  sep = c("! ", "... ")
)

data %>%
  cross() %>%
  map(lift(paste))

args <- data %>% cross_df()
args
args %>% transpose() %>% map_chr(~ invoke(paste, .))

# unwanted combinations
filter <- function(x, y) x >= y
cross2(1:5, 1:5, .filter = filter) %>% str()


# utilities ---------------------------------------------------------------
##
y <- list(0:10, 5.5)
y %>% every(is.numeric)
y %>% every(is.integer)

y %>% some(is.numeric)
y %>% some(is.integer)

##
x <- rerun(3, a = rbernoulli(1), b = sample(10))
x
x %>% keep("a")
x %>% discard("a")

1:10 %>% keep(~. > 5)

c(1:10, NULL) %>% compact()
list(1:10, NULL, 2) %>% compact()

##
x <- as.list(1:3)

x %>% append("a")
x %>% prepend("a")
x %>% prepend(list("a", "b"), before = 3)

##
inputs <- list(arg1 = "a", arg2 = "b")

splice(inputs, arg3 = c("c1", "c2")) %>% str()
list(inputs, arg3 = c("c1", "c2")) %>% str()
c(inputs, arg3 = c("c1", "c2")) %>% str()



# index into list  ------------------------------------------------------------
obj1 <- list("a", list(1, elt = "foobar"))
obj2 <- list("b", list(2, elt = "foobaz"))
x <- list(obj1, obj2)

# And now an accessor for these complex data structures:
my_element <- function(x) x[[2]]$elt

# The accessor can then be passed to pluck:
pluck(x, 1, my_element)
pluck(x, 2, my_element)

list(a = 2, b = 3, c = 4) %>% pluck("a")

##
is_even <- function(x) x %% 2 == 0

3:10 %>% detect(is_even)
3:10 %>% detect_index(is_even)

x <- list(
  list(1, foo = FALSE),
  list(2, foo = TRUE),
  list(3, foo = TRUE)
)

detect(x, "foo")
detect_index(x, "foo")

##
pos <- function(x) x >= 0
head_while(5:-5, pos)
tail_while(5:-5, negate(pos))

big <- function(x) x > 100
head_while(0:10, big)
tail_while(0:10, big)

##
x <- 1:5
rep_along(x, 1:2)
rep_along(x, 1)
list_along(x)

##
x <- list(x = 1:10, y = 4, z = list(a = 1, b = 2))
str(x)

str(list_modify(x, a = 1))
str(list_modify(x, z = 5))
str(list_modify(x, z = list(a = 1:5)))
str(list_modify(x, z = NULL))

# Combine values
str(list_merge(x, x = 11, z = list(a = 2:5, c = 3)))

# All these functions take dots with splicing. Use !!! or UQS() to
# splice a list of arguments:
l <- list(new = 1, y = NULL, z = 5)
str(list_modify(x, !!! l))

##
# Convert factors to characters
iris %>%
  modify_if(is.factor, as.character) %>%
  str()

# Specify which columns to map with a numeric vector of positions:
mtcars %>% modify_at(c(1, 4, 5), as.character) %>% str()

# Or with a vector of names:
mtcars %>% modify_at(c("cyl", "am"), as.character) %>% str()

list(x = rbernoulli(100), y = 1:100) %>%
  transpose() %>%
  modify_if("x", ~ update_list(., y = ~ y * 100)) %>%
  transpose() %>%
  simplify_all()

# Modify at specified depth ---------------------------
l1 <- list(
  obj1 = list(
    prop1 = list(param1 = 1:2, param2 = 3:4),
    prop2 = list(param1 = 5:6, param2 = 7:8)
  ),
  obj2 = list(
    prop1 = list(param1 = 9:10, param2 = 11:12),
    prop2 = list(param1 = 12:14, param2 = 15:17)
  )
)

# In the above list, "obj" is level 1, "prop" is level 2 and "param"
# is level 3. To apply sum() on all params, we map it at depth 3:
l1 %>% modify_depth(3, sum) %>% str()

# modify() lets us pluck the elements prop1/param2 in obj1 and obj2:
l1 %>% modify(c("prop1", "param2")) %>% str()

# But what if we want to pluck all param2 elements? Then we need to
# act at a lower level:
l1 %>% modify_depth(2, "param2") %>% str()

# modify_depth() can be with other purrr functions to make them operate at
# a lower level. Here we ask pmap() to map paste() simultaneously over all
# elements of the objects at the second level. paste() is effectively
# mapped at level 3.
l1 %>% modify_depth(2, ~ pmap(., paste, sep = " / ")) %>% str()
