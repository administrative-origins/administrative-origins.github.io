#--------------------
#Author: Lucas Cain
#Date Created: 10/05/2026
#Date Last Modified: 10/16/2026
#Purpose: We want a single Federal Register CSV, but the yearly The Federal Register CSVs have different column headers. Here we standardize those columns and combine all the CSVs into one.
#--------------------


####SETUP####
rm(list = ls())
#Groundhog for version control
install.packages("groundhog")
library("groundhog")

#Load packages as they were at date of creation
packages <- c('tidyverse','here','readxl')
groundhog.library(packages, '2026-09-25')

#setwd
setwd(here())

#Paths (the yearly registers live in Box, outside the project root)
base_dir   <- "C:/Users/Lucas/Box/Administrative Origins/Data Generation/1. Individual"
input_dir  <- file.path(base_dir, "Input", "a. Yearly Register", "CSVs")
output_dir <- file.path(base_dir, "Output")


####IDENTIFY COLUMN HEADERS####
#Yearly registers are .xlsx files; the "Old" subfolder holds superseded versions and is skipped
files <- list.files(input_dir, pattern = "^[0-9]{4}_official_register\\.xlsx$", full.names = TRUE)
files <- files[order(basename(files))]

#Clean a header: lower case, non-alphanumerics -> underscore (e.g. "Last Name" -> "last_name", "title 1" -> "title_1")
clean_names_simple <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9]+", "_") %>% str_remove_all("^_|_$")
}

#Read the first sheet only, everything as text so columns bind cleanly across years.
#(1907 and 1953 have a second sheet: 1907 Sheet2 is empty, 1953 Sheet2 re-lists records already in Sheet1.)
read_register <- function(path) {
  yr <- as.integer(str_extract(basename(path), "^[0-9]{4}"))
  df <- suppressMessages(read_excel(path, sheet = 1, col_types = "text", .name_repair = "minimal"))
  df <- df[, names(df) != "" & !is.na(names(df)), drop = FALSE] #drop unnamed stray columns
  names(df) <- clean_names_simple(names(df))
  df$file_year <- yr
  df
}

registers <- map(files, read_register)
names(registers) <- basename(files)

#Look at the distinct headers before standardizing
all_headers <- registers %>%
  imap_dfr(~ tibble(file = .y, column = names(.x))) %>%
  count(column, name = "n_files") %>%
  arrange(desc(n_files))
print(all_headers, n = Inf)


####STANDARDIZE COLUMN NAMES####
#Three header eras:
# 1817-1905: page, year, department, office, name, position, compensation, state_born, city_employed, state_employed (+ state_appointed, county_appointed, cong_district later)
# 1907-1921: last/first name, Dept, Title, Pay, Born, District, State/County appointed, County/State worked (1919-1921 already use the 1817-1905 style names)
# 1925-1959: name, position, state, district, compensation, bureau, title_1..title_7, department
#Names on the left are the old (cleaned) headers, on the right the standard header.
crosswalk <- c(
  #1907-1917 abbreviated headers
  last_name      = "lastname",
  first_name     = "firstname",
  department     = "dept",
  position       = "title",
  compensation   = "pay",
  state_born     = "born",
  cong_district  = "district",
  state_appointed  = "stateappointed",
  state_appointed  = "state_appointed",
  state_appointed  = "state", #1925+: state of appointment
  county_appointed = "county",
  city_employed    = "countyworked",
  city_employed    = "county_worked",
  state_employed   = "stateworked",
  state_employed   = "state_worked",
  #1919-1921 combined nation/state headers
  state_born       = "nation_state_born",
  state_appointed  = "nation_state_appointed",
  state_employed   = "nation_state_employed",
  #1919-1921 already-cleaned versions (2-letter codes, "INL" for foreign); other years get *_clean added later
  state_born_clean       = "st_born",
  state_appointed_clean  = "st_appointed",
  state_employed_clean   = "st_employed",
  cong_district_clean    = "cd",
  nation_born_clean      = "nation_born",
  nation_appointed_clean = "nation_appointed",
  nation_employed_clean  = "nation_employed"
)

#In the 1907 and 1909 sheets the Office and Title headers are swapped: "Office" holds the position (Clk, Mchnst, Lab)
#and "Title" holds the office (NavyYd, CustS, EngrAtLge). 1911 onward is labelled correctly.
swapped_office_title_years <- c(1907, 1909)

standardize_register <- function(df) {
  if (df$file_year[1] %in% swapped_office_title_years) {
    df <- df %>% rename(position = office, office = title)
  }
  #only rename columns that exist and whose target name is not already present
  for (new in unique(names(crosswalk))) {
    olds <- crosswalk[names(crosswalk) == new]
    for (old in olds) {
      if (old %in% names(df) && !(new %in% names(df))) {
        names(df)[names(df) == old] <- new
      }
    }
  }
  #1907-1921 split names into last/first; rebuild a combined name so every year has `name`
  if (!"name" %in% names(df) && all(c("last_name", "first_name") %in% names(df))) {
    df <- df %>% mutate(name = str_squish(str_c(first_name, last_name, sep = " ")))
  }
  #Years missing from the file take the year in the filename
  if (!"year" %in% names(df)) df$year <- NA_character_
  df %>% mutate(year = coalesce(year, as.character(file_year))) %>% select(-file_year)
}

#Remarks, standardized_department (1921) and abbreviations (1925) are not needed downstream
#NOTE: state_born, state_appointed, state_employed and cong_district hold the RAW entries for every year.
#The *_clean versions exist only for 1919 and 1921 (NA elsewhere) and will be filled in as the other years are cleaned.
registers_std <- map(registers, standardize_register) %>%
  map(~ select(.x, -any_of(c("remarks", "standardized_department", "abbreviations"))))

#Check: which columns remain unique to a single year after standardizing?
registers_std %>%
  imap_dfr(~ tibble(file = .y, column = names(.x))) %>%
  group_by(column) %>%
  summarise(n_files = n(), first_file = first(file), .groups = "drop") %>%
  arrange(n_files) %>%
  print(n = Inf)

#Bind everything; columns absent in a year are filled with NA
registers_all <- bind_rows(registers_std, .id = "source_file") %>%
  mutate(year = as.integer(year)) %>%
  relocate(source_file, year, page, name, last_name, first_name, department, bureau, office, position, compensation)

####COLLAPSE REPEATED ROWS####
#CHANGE (two steps; both leave a record of what they merged):
# 1. Exact repeats: rows identical in EVERY column are merged into one row (about 1,400 rows, 0.05%).
# 2. Rows that are identical in every column EXCEPT where the person worked (city_employed / state_employed) are merged into one row.
#    Example: a lighthouse superintendent listed once per light, each with a different city_employed.
#    The merged row keeps the first row's city_employed and state_employed, and ALL the distinct places are kept in
#    places_employed_list ("city, state | city, state | ..."; NA when there was only one place).
#New column n_collapsed = how many original rows each row stands for (1 = nothing merged). Nothing else is dropped.
#Rows that differ in any other column (name, position, pay, birthplace, ...) are never merged.
rows_before <- registers_all %>% count(year, name = "rows_before")
registers_all <- registers_all %>%
  mutate(.row = row_number()) %>%
  group_by(across(-.row)) %>%
  summarise(n_collapsed = n(), .row = min(.row), .groups = "drop")
rows_after_exact <- registers_all %>% count(year, name = "rows_after_exact")

place_columns <- c("city_employed", "state_employed")
registers_all <- registers_all %>%
  mutate(.place = if_else(is.na(city_employed) & is.na(state_employed), NA_character_,
                          str_c(coalesce(city_employed, "?"), ", ", coalesce(state_employed, "?")))) %>%
  group_by(across(-c(.row, n_collapsed, all_of(place_columns), .place))) %>%
  summarise(n_collapsed = sum(n_collapsed),
            city_employed = first(city_employed), state_employed = first(state_employed),
            places_employed_list = {p <- unique(.place[!is.na(.place)]); if (length(p) > 1) str_c(p, collapse = " | ") else NA_character_},
            .row = min(.row), .groups = "drop") %>%
  arrange(.row) %>%
  select(-.row) %>%
  relocate(city_employed, state_employed, .after = state_born) %>%
  relocate(n_collapsed, places_employed_list, .after = last_col())

collapse_summary <- rows_before %>%
  left_join(rows_after_exact, by = "year") %>%
  left_join(count(registers_all, year, name = "rows_after"), by = "year") %>%
  mutate(collapsed_exact = rows_before - rows_after_exact,
         collapsed_different_place = rows_after_exact - rows_after,
         rows_collapsed = rows_before - rows_after)
cat("Collapsed", sum(collapse_summary$collapsed_exact), "exact repeats and", sum(collapse_summary$collapsed_different_place),
    "rows differing only in place of employment;", nrow(registers_all), "rows remain\n")

#Sanity checks
stopifnot(!any(is.na(registers_all$year)))
registers_all %>% count(year) %>% print(n = Inf)
cat("Total rows:", nrow(registers_all), "\n")


####WRITE OUT####
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(registers_all, file.path(output_dir, "all_registers_1817_1959.csv"))
#Record of what the exact-repeat collapse removed, by year
dir.create(file.path(output_dir, "Tables"), showWarnings = FALSE, recursive = TRUE)
write_csv(collapse_summary, file.path(output_dir, "Tables", "exact_repeats_collapsed_by_year.csv"))
