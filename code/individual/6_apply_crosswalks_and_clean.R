#--------------------
#Author: Lucas Cain
#Date Created: 9/30/2026
#Date Last Modified: 10/30/2026
#Purpose: Here we apply the crosswalks from pervious steps to the federal register master. This standardizes the file across years and completes phase 1.
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
register_path <- file.path(base_dir, "Output", "all_registers_1817_1959.csv")        #output of script 1
crosswalk_dir <- file.path(base_dir, "Input", "b. Disambiguation Crosswalks")        #filled in by scripts 3-5
out_path      <- file.path(base_dir, "Output", "all_registers_1817_1959_clean.csv")
table_dir     <- file.path(base_dir, "Output", "Tables")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

#Years whose positions are abbreviated and therefore covered by the position crosswalk (must match script 2)
position_years <- 1907:1921


####APPLY CROSSWALKS####
registers <- read_csv(register_path, col_types = cols(.default = col_character())) %>%
  mutate(year = as.integer(year))

#Read a crosswalk as a named lookup: raw term -> cleaned value; for by_year crosswalks (position) raw term + year -> cleaned value,
#since the same abbreviation can mean different things in different years. Blank cleaned cells (not yet filled) become NA.
read_lookup <- function(name, by_year = FALSE) {
  cw <- read_csv(file.path(crosswalk_dir, str_c("crosswalk_", name, ".csv")),
                 col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
  keys <- if (by_year) c(name, "year") else name
  if (anyDuplicated(cw[keys])) stop("crosswalk_", name, ".csv has duplicated raw terms", if (by_year) " within a year")
  set_names(na_if(cw[[str_c(name, "_clean")]], ""), if (by_year) str_c(cw[[name]], "|", cw$year) else cw[[name]])
}
apply_lookup <- function(raw, lookup, year = NULL) unname(lookup[if (is.null(year)) raw else str_c(raw, "|", year)])  #NA raw (or a term missing from the crosswalk) -> NA

#1919 and 1921 already carry their own cleaned columns (2-letter codes / "INL"). Those are kept under a *_precleaned_1919_1921
#name for reference, and the cleaned columns below are rebuilt from the crosswalks for every year so they share one format.
precleaned <- c("state_born_clean", "state_appointed_clean", "state_employed_clean", "cong_district_clean")
registers <- registers %>%
  rename_with(~ str_c(str_remove(.x, "_clean$"), "_precleaned_1919_1921"), all_of(precleaned))

#FIX: DC APPOINTEES IN THE COMPACT REGISTERS (state_appointed blank, "DC" sitting in the district column)
#WHAT: for rows with a blank state_appointed and a raw cong_district of "DC", the appointing state is set to "DC".
#WHY:  1907 and 1911-1921 print a person born, appointed and employed in Washington as "D C D C D C". In those spreadsheets the three tokens
#      were assigned one column too far to the right, so the appointing "DC" ended up in cong_district (and the second "DC" in county), leaving
#      state_appointed empty. About 100,000 rows are affected (72-88% of the blank state_appointed rows in 1907 and 1911-1917, 59% in 1919, 49% in 1921).
#      The 1909 spreadsheet stores the same rows correctly (born DC, district DC, state_appointed DC), so this makes the other years match 1909.
#      Without it, state_appointed coverage drops to 82-88% in those years purely because DC appointees are missing.
#HOW:  the raw state_appointed and cong_district columns are left as transcribed. The corrected value is used only as the lookup input, and
#      state_appointed_dc_shift_fix flags every row it touched. A count by year is printed and saved to Output/Tables/dc_appointed_fix_by_year.csv.
#      NOT fixed here: county_appointed / county_worked / state_worked / city_employed for the same rows carry the same one-column shift.
registers <- registers %>%
  mutate(state_appointed_dc_shift_fix = is.na(state_appointed) & str_to_upper(str_squish(cong_district)) %in% "DC",
         state_appointed_for_lookup   = if_else(state_appointed_dc_shift_fix, "DC", state_appointed))
dc_fix_by_year <- registers %>%
  group_by(year) %>%
  summarise(n_rows = n(),
            state_appointed_blank_before = sum(is.na(state_appointed)),
            rows_fixed = sum(state_appointed_dc_shift_fix),
            pct_of_blank = round(100 * rows_fixed / pmax(state_appointed_blank_before, 1), 1),
            .groups = "drop") %>%
  filter(rows_fixed > 0)
print(dc_fix_by_year, n = Inf)
write_csv(dc_fix_by_year, file.path(table_dir, "dc_appointed_fix_by_year.csv"))

#One state crosswalk serves born, appointed and employed
state_lookup <- read_lookup("state")
stopifnot("the state crosswalk has no entry for DC, so the DC fix would not map" = !is.na(unname(state_lookup["DC"])))
registers <- registers %>%
  mutate(state_born_clean      = apply_lookup(state_born,      state_lookup),
         state_appointed_clean = apply_lookup(state_appointed_for_lookup, state_lookup),
         state_employed_clean  = apply_lookup(state_employed,  state_lookup),
         cong_district_clean   = apply_lookup(cong_district,   read_lookup("cong_district")))

#1881-1891 registers have no congressional district column. The numbers captured in cong_district for those years are the section headings of
#the Treasury field services (Internal Revenue collection districts, light-house districts, etc.), not congressional districts, so the cleaned
#district is blanked for those years. The raw cong_district column is left as transcribed.
no_cong_district_through <- 1891
registers <- registers %>%
  mutate(cong_district_clean = if_else(year <= no_cong_district_through, NA_character_, cong_district_clean))

#Positions: abbreviated only in position_years, so only those years go through the crosswalk.
#Other years are already written out in full, so their position is carried over as is (whitespace squished).
position_lookup <- read_lookup("position", by_year = TRUE)
registers <- registers %>%
  mutate(position_clean_source = case_when(is.na(position)           ~ NA_character_,
                                           year %in% position_years  ~ "crosswalk",
                                           TRUE                      ~ "raw"),
         position_clean = case_when(position_clean_source == "crosswalk" ~ apply_lookup(position, position_lookup, year),
                                    position_clean_source == "raw"       ~ str_squish(position)))

#Coverage: of the rows with a raw value, how many ended up with a cleaned value? (Remaining gaps are crosswalk entries not yet filled.)
coverage_pairs <- tribble(~raw, ~clean,
                          "state_born",      "state_born_clean",
                          "state_appointed_for_lookup", "state_appointed_clean",   #raw value after the DC fix above
                          "state_employed",  "state_employed_clean",
                          "cong_district",   "cong_district_clean",
                          "position",        "position_clean")
coverage <- pmap_dfr(coverage_pairs, function(raw, clean) {
  registers %>%
    filter(!is.na(.data[[raw]])) %>%
    group_by(year) %>%
    summarise(variable = raw, n_raw = n(), n_clean = sum(!is.na(.data[[clean]])), pct_clean = round(100 * n_clean / n_raw, 1), .groups = "drop")
}) %>%
  relocate(variable)
coverage %>%
  group_by(variable) %>%
  summarise(n_raw = sum(n_raw), n_clean = sum(n_clean), pct_clean = round(100 * n_clean / n_raw, 1)) %>%
  print()
write_csv(coverage, file.path(table_dir, "crosswalk_coverage_by_year.csv"))


####ADDITIONAL CLEANING####
##Yearly compensation
#Assumptions for converting pay rates to a yearly figure
hours_per_day  <- 8
days_per_week  <- 5
weeks_per_year <- 50
months_per_year <- 12
yearly_multiplier <- c(day   = days_per_week * weeks_per_year,
                       hour  = hours_per_day * days_per_week * weeks_per_year,
                       week  = weeks_per_year,
                       month = months_per_year,
                       year  = 1)

#Parse one compensation string into a yearly amount. Only a single amount with an optional rate unit is accepted:
#  "$1,200", "1200 per annum"            -> direct yearly salary
#  "$3.68pd", ".20 per hour", "$50pm"    -> rate x multiplier above ("cpd"/"cph" are cents per day/hour)
#  "$3to$5pd"                            -> midpoint of the range, then converted (counted as inferred)
#  optional trailing qualifiers (wae = when actually employed, max, &sub, &QM&L) are ignored
#Everything else is left NA: piecework, fees, "PayRank", several amounts ("9 per month and 9 rations"),
#footnote marks like "(1)", and "grade salary" pairs like "11 16,000".
#Yearly figures below min_yearly or above max_yearly are treated as implausible (typos, dropped units) and left NA.
min_yearly <- 100
max_yearly <- 100000
parse_compensation <- function(x) {
  s <- x %>%
    str_to_lower() %>%
    str_replace_all(c("½" = ".5", "¼" = ".25", "¾" = ".75", "⅓" = ".333333", "⅔" = ".666667", "⅛" = ".125")) %>%
    str_remove_all("[$,]") %>%
    str_squish()
  s <- if_else(str_detect(s, "[0-9] [0-9]"), NA_character_, str_remove_all(s, " "))  #two amounts side by side -> not parseable
  m <- str_match(s, "^([0-9]*\\.?[0-9]+)(?:to([0-9]*[.]?[0-9]+))?(perday|pd|p\\.d\\.?|cpd|perhour|ph|p\\.h\\.?|cph|perweek|pw|permonth|pm|perannum|peryear|p\\.a\\.?|pa)?(wae|max|maxwae|&sub|&qm&l|&qs&l)?$")
  is_range <- !is.na(m[, 3])
  amount <- if_else(is_range, (as.numeric(m[, 2]) + as.numeric(m[, 3])) / 2, as.numeric(m[, 2]))  #range -> midpoint
  unit <- case_when(is.na(m[, 4]) | m[, 4] %in% c("perannum", "peryear", "p.a.", "p.a", "pa") ~ "year",
                    m[, 4] %in% c("perday", "pd", "p.d.", "p.d", "cpd")   ~ "day",
                    m[, 4] %in% c("perhour", "ph", "p.h.", "p.h", "cph")  ~ "hour",
                    m[, 4] %in% c("perweek", "pw")                        ~ "week",
                    m[, 4] %in% c("permonth", "pm")                       ~ "month")
  amount <- if_else(m[, 4] %in% c("cpd", "cph"), amount / 100, amount)
  yearly <- amount * unname(yearly_multiplier[unit])
  yearly <- if_else(yearly >= min_yearly & yearly <= max_yearly, yearly, NA_real_)
  tibble(yearly = yearly,
         source = case_when(is.na(yearly) ~ NA_character_, unit == "year" & !is_range ~ "direct", TRUE ~ "inferred"))
}

#Parse each distinct string once, then map back to the rows
comp_values <- tibble(compensation = unique(registers$compensation)) %>%
  bind_cols(parse_compensation(.$compensation))
comp_idx <- match(registers$compensation, comp_values$compensation)
registers <- registers %>%
  mutate(yearly_compensation_inferred = comp_values$yearly[comp_idx],
         yearly_compensation_source   = comp_values$source[comp_idx])  #"direct" = stated yearly salary, "inferred" = converted from a day/hour/week/month rate

#Known transcription fix: 150 Paymaster's Clerks in 1861 are recorded as "700 per month" ($8,400 a year), far above every other clerk.
#We assume the intended rate was $700 a year and treat it as a stated yearly salary.
paymaster_1861 <- registers$year == 1861 & registers$position %in% "Paymaster's Clerk" & registers$compensation %in% "700 per month"
registers$yearly_compensation_inferred[paymaster_1861] <- 700
registers$yearly_compensation_source[paymaster_1861]   <- "direct"

#How much of the compensation field ended up with a yearly figure?
registers %>%
  filter(!is.na(compensation)) %>%
  count(yearly_compensation_source = coalesce(yearly_compensation_source, "not converted")) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()


####WRITE OUT####
write_csv(registers, out_path)
