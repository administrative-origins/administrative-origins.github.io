#--------------------
#Author: Lucas Cain
#Date Created: 10/16/2026
#Date Last Modified: 10/24/2026
#Purpose: Earlier hand codings (titles, birthplaces, state appointed) already resolve many of the abbreviations. Here we use them to pre-fill the empty crosswalks from script 2, so the next step only has to handle what is left.
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
crosswalk_dir <- "C:/Users/Lucas/Box/Administrative Origins/Data Generation/1. Individual/Input/b. Disambiguation Crosswalks"
hand_dir      <- "C:/Users/Lucas/Box/Administrative Origins/Data Generation/1. Individual/Input/c. Abbreviations/Hand Entered"


####READ HAND CODINGS####
#The hand-coded keys are the raw terms lower-cased with spaces and . , ' - stripped (e.g. "New York" -> "newyork"),
#so both sides are put through the same key before matching.
make_key <- function(x) str_remove_all(str_to_lower(x), "[[:space:]]|[.,'-]")

#Hand codings were saved in Windows-1252 (stray non-breaking spaces), so read as such and squish whitespace.
read_hand <- function(file) {
  read_csv(file.path(hand_dir, file), locale = locale(encoding = "Windows-1252"),
           col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE) %>%
    mutate(across(everything(), ~ str_squish(str_replace_all(.x, "[\u00a0\t]", " "))))
}

#Titles: Title_clean -> Title_real, Title Case so identical titles compare equal
titles <- read_hand("0721_Titles_reviewed.csv") %>%
  transmute(key = make_key(Title_clean), value = tools::toTitleCase(Title_real)) %>%
  filter(value != "", !key %in% c("", "_", "na"))

#States: birthplaces first, then the state-appointed file (extra terms only). Meaning is the full name.
#Appointed rows noted "County Appointed" are county names rather than states, so they are skipped.
birthplaces <- read_hand("0721_Birthplaces_reviewed.csv") %>%
  transmute(key = make_key(Born_clean), value = Meaning)
appointed <- read_hand("0721_StateAppointed_abv_no_plaintext_reviewed.csv") %>%
  filter(Note != "County Appointed") %>%
  transmute(key = make_key(StateAppointed_abv), value = Meaning)
states <- bind_rows(birthplaces, appointed) %>%
  filter(value != "", !key %in% c("", "_", "na"))

#One value per key (first one wins); report any keys whose hand codings disagree
check_conflicts <- function(lookup, label) {
  conflicts <- lookup %>% group_by(key) %>% filter(n_distinct(value) > 1) %>% ungroup()
  if (nrow(conflicts) > 0) { message(label, ": conflicting hand codings (first kept):"); print(conflicts) }
  distinct(lookup, key, .keep_all = TRUE)
}
titles <- check_conflicts(titles, "titles")
states <- check_conflicts(states, "states")


####FILL CROSSWALKS####
#Only blank cleaned cells are filled, so anything already filled in a crosswalk is never overwritten
fill_crosswalk <- function(name, lookup) {
  path <- file.path(crosswalk_dir, str_c("crosswalk_", name, ".csv"))
  clean_col <- str_c(name, "_clean")
  crosswalk <- read_csv(path, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)

  #Crosswalk rows are term-year pairs; the hand codings have no year, so for position a string gets the same value in every year it appears
  matched <- lookup$value[match(make_key(crosswalk[[name]]), lookup$key)]
  crosswalk[[clean_col]] <- if_else(crosswalk[[clean_col]] == "" & !is.na(matched), matched, crosswalk[[clean_col]])

  filled <- crosswalk[[clean_col]] != ""
  n <- as.numeric(crosswalk$n)
  message(name, ": filled ", sum(filled), " of ", nrow(crosswalk), " entries (",
          round(100 * sum(n[filled]) / sum(n), 1), "% of occurrences)")
  write_csv(crosswalk, path, na = "")
}

fill_crosswalk("position", titles)
fill_crosswalk("state", states)
