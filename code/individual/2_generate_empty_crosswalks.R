#--------------------
#Author: Lucas Cain
#Date Created: 10/05/2026
#Date Last Modified: 10/24/2026
#Purpose: The yearly registers have different abbreviations and spellings for the same thing. Here we generate all the crosswalks to disambiguate those abbreviations. These crosswalks will be filled by Claude in the next step.
#--------------------


####SETUP####
rm(list = ls())
#Groundhog for version control
install.packages("groundhog")
library("groundhog")

#Load packages as they were at date of creation
packages <- c('tidyverse','here')
groundhog.library(packages, '2026-09-25')

#setwd
setwd(here())

#Paths (Box, outside the project root)
base_dir      <- "C:/Users/Lucas/Box/Administrative Origins/Data Generation/1. Individual"
register_path <- file.path(base_dir, "Output", "all_registers_1817_1959.csv") #output of script 1
crosswalk_dir <- file.path(base_dir, "Input", "b. Disambiguation Crosswalks")
dir.create(crosswalk_dir, showWarnings = FALSE, recursive = TRUE)

#Fields to clean. The three state fields share one crosswalk (same abbreviations/spellings), the others get one each.
state_fields <- c("state_born", "state_employed", "state_appointed")
fields <- c("position", "cong_district")

#Set to TRUE only if you want to wipe crosswalks that have already been filled in
overwrite <- T


####GENERATE EMPTY CROSSWALKS####
#Only the raw columns are needed; read as text so nothing is recoded
#department/office/bureau are carried along as context to help disambiguate positions
context_fields <- c("department", "office", "bureau")
registers <- read_csv(register_path, col_select = all_of(c("year", fields, state_fields, context_fields)),
                      col_types = cols(.default = col_character())) %>%
  mutate(year = as.integer(year))

#Positions are only abbreviated in 1907-1921 (run together, e.g. "OfAudPO", "AsstChap"); other years are written out in full,
#so the position crosswalk is built from those years only. Everything else is left NA outside this window.
position_years <- 1907:1921
registers <- registers %>%
  mutate(position = if_else(year %in% position_years, position, NA_character_))

#Write a crosswalk from a tibble of raw terms (column `term`; also `year` if by_year). `name` is the crosswalk's term column (e.g. "position" or "state").
#If by_year, each string gets one row per year in which it appears, since the same abbreviation can mean different things in different years
#(positions only); otherwise one row per string.
#`context` is an optional tibble with one row per term (or term-year if by_year) of extra columns placed before the cleaned column.
write_crosswalk <- function(terms, name, context = NULL, by_year = FALSE) {
  keys <- if (by_year) c("term", "year") else "term"
  out_path <- file.path(crosswalk_dir, str_c("crosswalk_", name, ".csv"))
  if (file.exists(out_path) && !overwrite) {
    message("Skipping ", name, ": crosswalk already exists (set overwrite <- TRUE to replace)")
    return(invisible(NULL))
  }

  #One row per unique raw term (and year, if by_year), most frequent first. n helps prioritize; the cleaned column is left empty to be filled in.
  crosswalk <- terms %>%
    filter(!is.na(term)) %>%
    count(across(all_of(keys)), name = "n", sort = TRUE) %>%
    {if (is.null(context)) . else left_join(., context, by = keys)} %>%
    rename(!!name := term) %>%
    mutate(!!str_c(name, "_clean") := NA_character_)

  write_csv(crosswalk, out_path, na = "")
  message(name, ": ", nrow(crosswalk), if (by_year) " unique term-years -> " else " unique terms -> ", out_path)
}

#For each raw position and year, the departments/offices/bureaus it appears under, most common first, as "value (count); ..."
#Columns that are empty for the position years (e.g. bureau, which only exists from 1925) are dropped.
top_context <- function(df, col, k = 5) {
  df %>%
    filter(!is.na(term), !is.na(.data[[col]])) %>%
    count(term, year, value = .data[[col]], name = "n_value") %>%
    arrange(term, year, desc(n_value)) %>%
    group_by(term, year) %>%
    slice_head(n = k) %>%
    summarise(!!str_c(col, "_context") := str_c(value, " (", n_value, ")", collapse = "; "), .groups = "drop")
}
position_rows <- registers %>% mutate(term = position)
position_context <- reduce(map(context_fields, ~ top_context(position_rows, .x)), full_join, by = c("term", "year")) %>%
  select(where(~ !all(is.na(.x))))

#Single-field crosswalks (position gets its context columns)
write_crosswalk(transmute(registers, term = position, year), "position", context = position_context, by_year = TRUE)
write_crosswalk(transmute(registers, term = cong_district), "cong_district")

#Combined state crosswalk: terms pooled across born/employed/appointed, n is the total across the three
state_terms <- registers %>%
  select(all_of(state_fields)) %>%
  pivot_longer(everything(), values_to = "term") %>%
  select(term)
write_crosswalk(state_terms, "state")


####CITY -> STATE CROSSWALK####
#Many rows have a city_employed but no state_employed (e.g. 6,575 rows in 1865, when the register lists city but has no state column). This crosswalk
#lets the state be filled in from the city. Only rows with a city and a blank raw state_employed are included; rows that already have a state are not touched.
#Some city names are ambiguous without more information (Springfield, Columbia, Portland...), so each city carries context columns showing what
#else is known about the people listed under it: the departments/offices they work in and the states they were appointed from / born in.
#Fill state_employed_clean with the state (same format as the state crosswalk's state_clean) or leave it blank if it can't be determined.
#All register years are included (city_years). 1907+ city_employed holds compact run-together place names (e.g. "DC", "NavyYdNewYork", "KansasCity").
#Cities already filled in crosswalk_state_from_city_emp_filled.csv keep their filled value in the new crosswalk (carried over by exact city string), so nothing hand-filled
#is lost when the crosswalk is regenerated. n, the years and the context columns are computed over all years.
city_years <- 1817:1959
overwrite_city <- FALSE   #kept separate from `overwrite` so re-running this script never wipes a filled city crosswalk by accident
city_path <- file.path(crosswalk_dir, "crosswalk_state_from_city_emp.csv")
city_filled_path <- file.path(crosswalk_dir, "crosswalk_state_from_city_emp_filled.csv")

if (file.exists(city_path) && !overwrite_city) {
  message("Skipping city: crosswalk already exists (set overwrite_city <- TRUE to replace)")
} else {
  city_rows <- read_csv(register_path,
                        col_select = all_of(c("year", "city_employed", "state_employed", "state_appointed", "state_born", "department", "office")),
                        col_types = cols(.default = col_character())) %>%
    mutate(year = as.integer(year)) %>%
    filter(year %in% city_years, !is.na(city_employed), is.na(state_employed))

  #For each city, the most common values of a context column as "value (count); ..."
  city_top_context <- function(col, k = 5) {
    city_rows %>%
      filter(!is.na(.data[[col]])) %>%
      count(city_employed, value = .data[[col]], name = "n_value") %>%
      arrange(city_employed, desc(n_value)) %>%
      group_by(city_employed) %>%
      slice_head(n = k) %>%
      summarise(!!str_c(col, "_context") := str_c(value, " (", n_value, ")", collapse = "; "), .groups = "drop")
  }

  city_crosswalk <- city_rows %>%
    group_by(city_employed) %>%
    summarise(n = n(), first_year = min(year), last_year = max(year), .groups = "drop") %>%
    arrange(desc(n)) %>%
    left_join(city_top_context("state_appointed"), by = "city_employed") %>%
    left_join(city_top_context("state_born"),      by = "city_employed") %>%
    left_join(city_top_context("department", k = 3), by = "city_employed") %>%
    left_join(city_top_context("office", k = 3),     by = "city_employed") %>%
    mutate(state_employed_clean = NA_character_)

  #Carry over the hand-filled states from the filled crosswalk
  if (file.exists(city_filled_path)) {
    already_filled <- read_csv(city_filled_path, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE) %>%
      transmute(city_employed, filled_state = na_if(str_squish(state_employed_clean), "")) %>%
      filter(!is.na(filled_state))
    if (anyDuplicated(already_filled$city_employed)) stop("crosswalk_state_from_city_emp_filled.csv has duplicated cities")
    city_crosswalk <- city_crosswalk %>%
      left_join(already_filled, by = "city_employed") %>%
      mutate(state_employed_clean = filled_state) %>%
      select(-filled_state)
    lost <- setdiff(already_filled$city_employed, city_crosswalk$city_employed)
    if (length(lost) > 0) { message("city: filled cities no longer in the register (not carried over):"); print(lost) }
  }

  write_csv(city_crosswalk, city_path, na = "")
  message("city: ", nrow(city_crosswalk), " unique cities (", sum(!is.na(city_crosswalk$state_employed_clean)), " already filled, carried over from ",
          basename(city_filled_path), "; ", nrow(city_rows), " rows with a city but no state, ", min(city_years), "-", max(city_years), ") -> ", city_path)
}
