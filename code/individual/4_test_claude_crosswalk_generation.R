#--------------------
#Author: Lucas Cain
#Date Created: 9/30/2026
#Date Last Modified: 10/26/2026
#Purpose:  Here we test Claude's ability to fill empty crosswalks generated in the last step against human coders. We (1) draw a random sample of still-unfilled crosswalk entries for the coders to fill in, then (2) read the filled samples back in and measure inter-coder reliability.
#--------------------


####SETUP####
rm(list = ls())
#Groundhog for version control
install.packages("groundhog")
library("groundhog")

#Load packages as they were at date of creation
packages <- c('tidyverse','here','irr')
groundhog.library(packages, '2026-09-25')

#setwd
setwd(here())

#Paths (Box, outside the project root)
base_dir      <- "C:/Users/Lucas/Box/Administrative Origins/Data Generation/1. Individual"
crosswalk_dir <- file.path(base_dir, "Input", "b. Disambiguation Crosswalks")
sample_dir    <- file.path(base_dir, "Input", "d. Reliability Samples", "Blank samples")  #templates to hand to coders
coded_dir     <- file.path(base_dir, "Input", "d. Reliability Samples", "Coded samples")  #filled files, one per coder and field
figure_dir    <- file.path(base_dir, "Output", "Figures")
table_dir     <- file.path(base_dir, "Output", "Tables")
walk(c(sample_dir, coded_dir, figure_dir, table_dir), dir.create, showWarnings = FALSE, recursive = TRUE)

#Sample sizes by crosswalk (crosswalk name -> number of entries drawn)
sample_sizes <- c(state = 50, cong_district = 50, position = 150)
#Context columns shown to coders, by crosswalk (other columns are hidden)
#Position entries are term-year pairs (script 2), so the year is shown for positions
context_cols <- list(state = "n", cong_district = "n", position = c("year", "n", "department_context", "office_context"))

#Coders whose files are LLMs; everything else is treated as a human coder (names are the <coder> part of the filename)
llm_coders <- c("claude")

#Set to TRUE only if you want to redraw samples; redrawing invalidates any coding already done on the old samples
redraw_samples <- F
set.seed(20261016)


####GENERATE RANDOM SAMPLES####
#Draw uniformly at random from the entries (term-years for positions) whose cleaned value is still blank
draw_sample <- function(field) {
  out_path <- file.path(sample_dir, str_c("sample_", field, ".csv"))
  if (file.exists(out_path) && !redraw_samples) {
    message("Skipping ", field, ": sample already exists (set redraw_samples <- TRUE to redraw)")
    return(invisible(NULL))
  }

  clean_col <- str_c(field, "_clean")
  crosswalk <- read_csv(file.path(crosswalk_dir, str_c("crosswalk_", field, ".csv")),
                        col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
  unfilled <- crosswalk %>% filter(.data[[clean_col]] == "")
  if (nrow(unfilled) < sample_sizes[[field]]) stop(field, ": only ", nrow(unfilled), " unfilled entries, fewer than the sample size")

  drawn <- unfilled %>%
    slice_sample(n = sample_sizes[[field]]) %>%
    mutate(sample_id = str_c(field, "_", str_pad(row_number(), 3, pad = "0"))) %>%
    select(sample_id, all_of(c(field, context_cols[[field]], clean_col)))

  write_csv(drawn, out_path, na = "")
  message(field, ": drew ", nrow(drawn), " of ", nrow(unfilled), " unfilled entries -> ", out_path)
}

walk(names(sample_sizes), draw_sample)


####READ FILLED SAMPLES####
#Each coder has a folder in coded_dir named "Coder <k> (<name>)" (e.g. "Coder 3 (Claude)") holding one file per field, named
#sample_<field>[_<suffix>].csv (e.g. sample_position_C.csv). The coder is the name in parentheses; files keep sample_id and <field>_clean.
file_pattern <- str_c("^sample_(", str_c(names(sample_sizes), collapse = "|"), ")(_.*)?\\.csv$")
coded_files <- list.files(coded_dir, pattern = file_pattern, full.names = TRUE, recursive = TRUE)
if (length(coded_files) == 0) {
  stop("No coded samples found in ", coded_dir, " yet. Add the filled files (<coder folder>/sample_<field>.csv) and rerun for the reliability analysis.")
}

#Compare after light normalization (case, whitespace, . , ' - ) so formatting differences are not counted as disagreement.
make_key <- function(x) str_remove_all(str_to_lower(x), "[[:space:]]|[.,'-]")

coded <- map_dfr(coded_files, function(path) {
  field <- str_match(basename(path), file_pattern)[2]
  folder <- basename(dirname(path))
  coder <- str_to_lower(coalesce(str_match(folder, "\\((.+)\\)")[2], folder))  #"Coder 3 (Claude)" -> "claude"
  x <- read_csv(path, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
  file_has_coarse <- str_c(field, "_clean_coarse") %in% names(x)
  x %>%
    transmute(field = field, coder = coder, sample_id, has_coarse = file_has_coarse,
              response = na_if(str_squish(.data[[str_c(field, "_clean")]]), ""),
              #state and cong_district files may also carry a coarse column (<field>_clean_coarse): the district number(s) /
              #"At Large" / "Other", or the state, territory or nation / "Other". Coders without one are left NA at that level.
              response_coarse = if (file_has_coarse) na_if(str_squish(.data[[str_c(field, "_clean_coarse")]]), "") else NA_character_)
})

#Stack the two levels of coding: "detailed" (<field>_clean, every field) and "coarse" (state and cong_district only)
coded <- bind_rows(mutate(coded, level = "detailed"),
                   coded %>% filter(has_coarse) %>% mutate(level = "coarse", response = response_coarse)) %>%
  select(-response_coarse, -has_coarse) %>%
  mutate(response_key = make_key(response),
         coder_type = if_else(coder %in% llm_coders, "LLM", "human"))

#Attach the raw term, its register year (positions only), and any duplicate-sample checks
terms <- map_dfr(names(sample_sizes), function(field) {
  read_csv(file.path(sample_dir, str_c("sample_", field, ".csv")), col_types = cols(.default = col_character()),
           na = character(), show_col_types = FALSE) %>%
    transmute(field = field, sample_id, term = .data[[field]], year = if ("year" %in% names(.)) year else NA_character_)
})
stopifnot(!any(duplicated(coded[c("field", "level", "coder", "sample_id")])))
coded <- coded %>% left_join(terms, by = c("field", "sample_id"))

#How complete is each coder's file?
completeness <- coded %>%
  group_by(field, level, coder, coder_type) %>%
  summarise(n_items = n(), n_filled = sum(!is.na(response)), pct_filled = round(100 * n_filled / n_items, 1), .groups = "drop")
print(completeness, n = Inf)


####DISAGREEMENTS####
#Items where coders disagree, for adjudication. The output file is hand-marked with an "Important?" column (1 = substantive difference,
#0 = trivial/equivalent), so we never overwrite it: existing marks are read back in and carried over, matched on field, level and sample_id.
#New disagreements (e.g. from a newly added coder) get a blank mark to be filled in by hand.
disagreements_path <- file.path(table_dir, "intercoder_disagreements.csv")

disagreements <- coded %>%
  filter(!is.na(response_key)) %>%
  group_by(field, level, sample_id, term, year) %>%
  filter(n_distinct(response_key) > 1) %>%
  ungroup() %>%
  select(field, level, sample_id, term, year, coder, response) %>%
  pivot_wider(names_from = coder, values_from = response) %>%
  arrange(field, level, sample_id)

if (file.exists(disagreements_path)) {
  marks <- read_csv(disagreements_path, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE) %>%
    transmute(field, level, sample_id, `Important?` = na_if(str_squish(`Important?`), ""))
  stopifnot(!any(duplicated(marks[c("field", "level", "sample_id")])))
  dropped <- anti_join(marks, disagreements, by = c("field", "level", "sample_id")) %>% filter(!is.na(`Important?`))
  if (nrow(dropped) > 0) message(nrow(dropped), " marked item(s) in the existing file are no longer disagreements and are dropped from it (coding changed?)")
  disagreements <- left_join(disagreements, marks, by = c("field", "level", "sample_id"))
} else {
  disagreements <- mutate(disagreements, `Important?` = NA_character_)
}
stopifnot(all(disagreements$`Important?` %in% c("0", "1", NA)))
n_unmarked <- sum(is.na(disagreements$`Important?`))
if (n_unmarked > 0) message(n_unmarked, " disagreement(s) have no Important? mark; they are excluded from the important-differences analysis until marked")

#Write the marked file straight away (before anything else can fail), preserving the marks
write_csv(disagreements, disagreements_path, na = "")

#Version of the data where unimportant differences count as agreement: on items marked 0, every coder's answer is collapsed to a
#single value. Items marked 1 keep the coders' answers; unmarked disagreements are dropped. Items without a disagreement are unchanged.
important_marks <- select(disagreements, field, level, sample_id, important = `Important?`)
coded_important <- coded %>%
  anti_join(filter(important_marks, is.na(important)), by = c("field", "level", "sample_id")) %>%
  left_join(important_marks, by = c("field", "level", "sample_id")) %>%
  mutate(response     = if_else(!is.na(response) & important %in% "0", "(equivalent)", response),
         response_key = if_else(!is.na(response_key) & important %in% "0", "(equivalent)", response_key)) %>%
  select(-important)


####COMPARE CLASSIFICATION####
#Cohen's kappa for two coders on nominal categories (free-text answers are the categories)
cohens_kappa <- function(a, b) {
  p_o <- mean(a == b)
  cats <- union(a, b)
  p_e <- sum(map_dbl(cats, ~ mean(a == .x) * mean(b == .x)))
  if (p_e == 1) NA_real_ else (p_o - p_e) / (1 - p_e)
}


#All agreement statistics for one version of the data (all differences, or important differences only)
compute_agreement <- function(data) {
  #For each pairwise, what proportion of de-abbreviations are the same?
  #Only items both coders filled in are compared; n_compared shows how many that is.
  pairs <- data %>%
    select(field, level, coder, coder_type, sample_id, response, response_key)
  pairwise <- pairs %>%
    inner_join(pairs, by = c("field", "level", "sample_id"), suffix = c("_1", "_2"), relationship = "many-to-many") %>%
    filter(coder_1 < coder_2, !is.na(response_key_1), !is.na(response_key_2)) %>%
    group_by(field, level, coder_1, coder_2, coder_type_1, coder_type_2) %>%
    summarise(n_compared      = n(),
              pct_agree       = round(100 * mean(response_key_1 == response_key_2), 1),
              pct_agree_exact = round(100 * mean(response_1 == response_2), 1),
              kappa           = round(cohens_kappa(response_key_1, response_key_2), 3),
              .groups = "drop") %>%
    mutate(pair_type = case_when(coder_type_1 == "human" & coder_type_2 == "human" ~ "human-human",
                                 coder_type_1 == "LLM"   & coder_type_2 == "LLM"   ~ "LLM-LLM",
                                 TRUE ~ "human-LLM"))

  #Agreement among all coders at once: Krippendorff's alpha (nominal; handles unfilled items)
  overall <- data %>%
    group_by(field, level) %>%
    group_modify(function(df, key) {
      wide <- df %>% select(coder, sample_id, response_key) %>% pivot_wider(names_from = sample_id, values_from = response_key)
      wide <- wide %>% filter(if_any(-coder, ~ !is.na(.x)))  #drop coders with nothing at this level
      if (nrow(wide) < 2) return(tibble(n_coders = nrow(wide), n_items = ncol(wide) - 1L, krippendorff_alpha = NA_real_))
      ratings <- wide %>% select(-coder) %>% as.matrix()
      ratings <- matrix(as.integer(factor(ratings)), nrow = nrow(ratings))  #category codes, NA stays NA
      tibble(n_coders = nrow(ratings), n_items = ncol(ratings),
             krippendorff_alpha = round(kripp.alpha(ratings, method = "nominal")$value, 3))
    }) %>%
    ungroup()

  #Are humans closer to each other than to the LLM?
  by_pair_type <- pairwise %>%
    group_by(field, level, pair_type) %>%
    summarise(n_pairs = n(), mean_pct_agree = round(mean(pct_agree), 1), mean_kappa = round(mean(kappa, na.rm = TRUE), 3), .groups = "drop")

  list(pairwise = pairwise, overall = overall, by_pair_type = by_pair_type)
}

all_diffs <- compute_agreement(coded)
print(all_diffs$pairwise, n = Inf); print(all_diffs$overall); print(all_diffs$by_pair_type, n = Inf)

#Same statistics counting only important differences (unimportant differences count as agreement)
important_diffs <- compute_agreement(coded_important)
message("Agreement on important differences only:")
print(important_diffs$pairwise, n = Inf); print(important_diffs$overall); print(important_diffs$by_pair_type, n = Inf)

#Save pairwise agreement heatmap, one panel per crosswalk and level of coding, for all vs. important-only differences
heatmap_data <- bind_rows(mutate(all_diffs$pairwise, basis = "All differences"),
                          mutate(important_diffs$pairwise, basis = "Important differences only")) %>%
  select(basis, field, level, coder_1, coder_2, pct_agree)
agreement_plot <- heatmap_data %>%
  bind_rows(rename(heatmap_data, coder_1 = coder_2, coder_2 = coder_1)) %>%
  ggplot(aes(coder_1, coder_2, fill = pct_agree)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(pct_agree)), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#2171b5", limits = c(0, 100), name = "% agree") +
  facet_grid(basis + level ~ field) +
  labs(x = NULL, y = NULL, title = "Pairwise inter-coder agreement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(figure_dir, "intercoder_agreement.png"), agreement_plot, width = 10, height = 10, dpi = 300)


####OUTPUT####
#(intercoder_disagreements.csv is written earlier, with its hand-marked Important? column preserved)
write_csv(completeness,                  file.path(table_dir, "intercoder_completeness.csv"))
write_csv(all_diffs$pairwise,            file.path(table_dir, "intercoder_pairwise_agreement.csv"))
write_csv(all_diffs$overall,             file.path(table_dir, "intercoder_overall_alpha.csv"))
write_csv(all_diffs$by_pair_type,        file.path(table_dir, "intercoder_agreement_by_pair_type.csv"))
write_csv(important_diffs$pairwise,      file.path(table_dir, "intercoder_pairwise_agreement_important.csv"))
write_csv(important_diffs$overall,       file.path(table_dir, "intercoder_overall_alpha_important.csv"))
write_csv(important_diffs$by_pair_type,  file.path(table_dir, "intercoder_agreement_by_pair_type_important.csv"))