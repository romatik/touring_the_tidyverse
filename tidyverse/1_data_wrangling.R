# skimr -------------------------------------------------------------------
library(skimr)

skimr::skim(dplyr::starwars)

skimr::skim(iris) %>% summary()

skimr::skim(iris, dplyr::starts_with("Petal"))

iris %>% dplyr::group_by(Species) %>% skim()


# janitor -----------------------------------------------------------------
roster_raw <- readxl::read_excel("tidyverse/dirty_data.xlsx")

dplyr::glimpse(roster_raw)

roster <- roster_raw %>%
  janitor::clean_names() %>%
  janitor::remove_empty(c("rows", "cols")) %>%
  dplyr::mutate(hire_date = janitor::excel_numeric_to_date(hire_date),
                cert = dplyr::coalesce(certification_9, certification_10)) %>% # from dplyr
  dplyr::select(-certification_9, -certification_10) # drop unwanted columns

roster %>% janitor::get_dupes(first_name, last_name)


# tabyl -------------------------------------------------------------------
roster %>%
  janitor::tabyl(subject)

roster %>%
  dplyr::filter(hire_date > as.Date("1950-01-01")) %>%
  janitor::tabyl(employee_status, full_time)

roster %>%
  janitor::tabyl(full_time, subject, employee_status, show_missing_levels = FALSE)

roster %>%
  janitor::tabyl(employee_status, full_time) %>%
  janitor::adorn_totals("row") %>%
  janitor::adorn_totals("col") %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting() %>%
  janitor::adorn_ns() %>%
  janitor::adorn_title("combined")
