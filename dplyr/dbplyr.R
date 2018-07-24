# https://www.nielsenmark.us/2018/07/07/connecting-r-to-postgresql-on-linux/

library(RPostgreSQL)
library(dplyr)

# https://stackoverflow.com/questions/37694987/connecting-to-postgresql-in-a-docker-container-from-outside#37704532
db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     dbname = "postgres",
                     host = "localhost",
                     port = 5432L,
                     user = "postgres",
                     password = "mysecretpassword")

db_sqlite <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")


dplyr::copy_to(db, ggplot2::msleep, "msleep")
dplyr::copy_to(db_sqlite, ggplot2::msleep, "msleep")


msleep_tbl_pg <- dplyr::tbl(db, "msleep")
msleep_tbl_sqlite <- dplyr::tbl(db_sqlite, "msleep")

### selecting
msleep_tbl_pg %>%
  select(matches("o.+er")) %>%
  glimpse

msleep_tbl_sqlite %>%
  select(matches("o.+er")) %>%
  glimpse

### mutating
msleep_tbl_pg %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr =
           case_when(
             sleep_total > 13 ~ "very long",
             sleep_total > 10 ~ "long",
             sleep_total > 7 ~ "limited",
             TRUE ~ "short"))

msleep_tbl_sqlite %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr =
           case_when(
             sleep_total > 13 ~ "very long",
             sleep_total > 10 ~ "long",
             sleep_total > 7 ~ "limited",
             TRUE ~ "short"))

### filtering
msleep_tbl_pg %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(sleep_total, sleep_rem), all_vars(.>5))

msleep_tbl_sqlite %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(sleep_total, sleep_rem), all_vars(.>5))

### summarizing
msleep_tbl_sqlite %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

msleep_tbl_pg %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

### scoped functions work
msleep_tbl_sqlite %>%
  group_by(vore) %>%
  summarise_if(is.numeric, funs(avg = mean(.)), na.rm = TRUE)

msleep_tbl_pg %>%
  group_by(vore) %>%
  summarise_if(is.numeric, funs(avg = mean(.)), na.rm = TRUE)

### implementation is different
msleep_tbl_sqlite %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  explain()

msleep_tbl_pg %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  explain()

### not everything works...
msleep_tbl_pg %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)

### but you can always use native DB-backends functions
# more info - http://db.rstudio.com/advanced/translation/
msleep_tbl_pg %>%
  filter(name %like% "Cheetah")

### once heavy work is done, you can collect
msleep_tbl_sqlite %>%
  group_by(vore) %>%
  summarise_if(is.numeric, funs(avg = mean(.)), na.rm = TRUE) %>%
  collect()

msleep_tbl_pg %>%
  group_by(vore) %>%
  summarise_if(is.numeric, funs(avg = mean(.)), na.rm = TRUE) %>%
  collect()

### example of compute()
msleep_tbl_sqlite %>%
  mutate(anotherwt = bodywt,
         onemorewt = anotherwt^2) %>%
  compute()

