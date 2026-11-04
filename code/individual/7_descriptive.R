#--------------------
#Author: Lucas Cain
#Date Created: 9/30/2026
#Date Last Modified: 11/03/2026
#Purpose: Here we tabulate and plot some descriptive statistics
#--------------------


####SETUP####
rm(list = ls())
#Groundhog for version control
install.packages("groundhog")
library("groundhog")

#Load packages as they were at date of creation
packages <- c('tidyverse','here','scales','ggrepel','maps','mapproj','gganimate','gifski')
groundhog.library(packages, '2026-09-25')

#setwd
setwd(here())

#Paths (Box, outside the project root)
base_dir      <- "C:/Users/Lucas/Box/Administrative Origins/Data Generation/1. Individual"
register_path <- file.path(base_dir, "Output", "all_registers_1817_1959_clean.csv")  #output of script 6
cpi_path      <- file.path(base_dir, "Input", "e. Price Index", "officer_williamson_cpi.csv")  #annual CPI, 1982-84 = 100
wage_path     <- file.path(base_dir, "Input", "f. Wage Series", "officer_williamson_wages.csv")  #US wage benchmarks
hours_path    <- file.path(base_dir, "Input", "f. Wage Series", "weekly_hours_manufacturing.csv")  #weekly hours worked, benchmark years
figure_dir    <- file.path(base_dir, "Output", "Figures")
table_dir     <- file.path(base_dir, "Output", "Tables")
walk(c(figure_dir, table_dir), dir.create, showWarnings = FALSE, recursive = TRUE)


####SETTINGS####
#Dollar year that real (inflation-adjusted) pay is expressed in
real_dollar_year <- 2025

#Converting the US wage benchmarks to yearly dollars.
#Production-worker pay uses the hours actually worked in each year: weekly manufacturing hours (Whaples/EH.Net, interpolated between
#benchmark years, held flat outside 1830-1958) x wage_weeks_per_year. The register pay conversion in script 6 keeps its flat 8-hour/5-day assumption.
wage_weeks_per_year <- 50
#Unskilled daily wage x days worked per year, where days per week = weekly hours / wage_workday_hours, capped at a six-day week
wage_workday_hours      <- 8
wage_max_days_per_week  <- 6
#The unskilled-labor series is only an index (1860 = 100). Put the unskilled DAILY wage in 1860 dollars here to draw it;
#while this is NA the unskilled line is left off the plot.
unskilled_daily_wage_1860 <- 1.01


####THEME####
plot_font      <- ""          #"" = default font; e.g. "serif" or "Arial"
base_size      <- 12          #base text size for all plots
fig_width      <- 9           #inches
fig_height     <- 5.5
fig_dpi        <- 300
line_width     <- 0.9
point_size     <- 1.6
ink_primary    <- "#0b0b0b"   #titles and axis text
ink_secondary  <- "#52514e"   #subtitles, captions, legend text
grid_colour    <- "#e6e5e1"
surface_colour <- "#ffffff"
x_breaks       <- seq(1820, 1960, by = 20)

#Categorical colours, assigned in this order (colour-blind-checked set). The two greys below are for "other" and "missing".
register_colors <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
neutral_colour  <- "#d4d2cb"   #light grey: "missing" / "no amount"
other_colour    <- "#8c8a82"   #darker grey: "other"
footnote_colour <- "#c8b58a"   #tan: pay given only as a footnote reference

theme_register <- function(size = base_size) {
  theme_minimal(base_size = size, base_family = plot_font) +
    theme(plot.background   = element_rect(fill = surface_colour, colour = NA),
          panel.background  = element_rect(fill = surface_colour, colour = NA),
          panel.grid.major  = element_line(colour = grid_colour, linewidth = 0.4),
          panel.grid.minor  = element_blank(),
          axis.title        = element_text(colour = ink_secondary),
          axis.text         = element_text(colour = ink_secondary),
          plot.title        = element_text(colour = ink_primary, face = "bold", size = size * 1.2, margin = margin(b = 4)),
          plot.subtitle     = element_text(colour = ink_secondary, margin = margin(b = 10)),
          plot.caption      = element_text(colour = ink_secondary, size = size * 0.75, hjust = 0, margin = margin(t = 10)),
          plot.title.position   = "plot",
          plot.caption.position = "plot",
          legend.position   = "bottom",
          legend.title      = element_blank(),
          legend.text       = element_text(colour = ink_secondary),
          plot.margin       = margin(12, 16, 10, 12))
}
theme_set(theme_register())

scale_colour_register <- function(...) scale_colour_manual(values = register_colors, ...)
scale_fill_register   <- function(...) scale_fill_manual(values = register_colors, ...)
scale_x_year          <- function(...) scale_x_continuous(breaks = x_breaks, ...)

save_plot  <- function(plot, name, width = fig_width, height = fig_height) {
  ggsave(file.path(figure_dir, str_c(name, ".png")), plot, width = width, height = height, dpi = fig_dpi, bg = surface_colour)
}
save_table <- function(df, name) write_csv(df, file.path(table_dir, str_c(name, ".csv")))


####READ DATA####
registers <- read_csv(register_path, col_types = cols(.default = col_character()),
                      col_select = c(year, department, state_born_clean, state_appointed_clean, state_employed_clean, cong_district, cong_district_clean, position_clean,
                                     compensation, yearly_compensation_inferred, yearly_compensation_source)) %>%
  mutate(year = as.integer(year),
         yearly_compensation_inferred = as.numeric(yearly_compensation_inferred))

#A raw district of "_" was left intentionally blank in the register, so it counts as covered (recorded) even though its cleaned value is NA.
#"At Large" entries already carry a cleaned value ("At Large"), so they count as covered through cong_district_clean.
registers <- registers %>% mutate(cong_district_recorded = !is.na(cong_district_clean) | cong_district %in% "_")

#Employees appointed from an at-large state or a territory/possession have no district number to record, so a blank there counts as covered.
#Which state-years are at-large is read off the registers: in a year when most state-appointed employees do carry a district (>= 50%),
#a state or territory with at least 30 employees and under 5% recorded is treated as at large / delegate-only.
#Foreign countries, "Army", "International" and the Military Academy are not included, and neither is DC (it is a district, not a state or territory).
territories <- c("Alaska", "Arizona", "Hawaii", "Hawaii Territory", "Indian Territory", "New Mexico", "Oklahoma", "Philippine Islands",
                 "Puerto Rico", "Guam", "Samoa", "Panama")
at_large_eligible <- c(state.name, territories)
year_share <- registers %>%
  filter(!is.na(state_appointed_clean), state_appointed_clean != "DC") %>%
  group_by(year) %>% summarise(year_recorded = mean(cong_district_recorded), .groups = "drop")
at_large_state_years <- registers %>%
  filter(state_appointed_clean %in% at_large_eligible) %>%
  group_by(year, state_appointed_clean) %>% summarise(n = n(), share_recorded = mean(cong_district_recorded), .groups = "drop") %>%
  left_join(year_share, by = "year") %>%
  filter(year_recorded >= 0.5, n >= 30, share_recorded < 0.05) %>%
  transmute(year, state_appointed_clean, n, at_large_inferred = TRUE)
save_table(at_large_state_years %>% select(-at_large_inferred), "at_large_state_years_inferred")
registers <- registers %>%
  left_join(at_large_state_years %>% select(year, state_appointed_clean, at_large_inferred), by = c("year", "state_appointed_clean")) %>%
  mutate(cong_district_covered = cong_district_recorded | coalesce(at_large_inferred, FALSE)) %>%
  select(-at_large_inferred)

#Shares below depend on the crosswalks: a place that is not (yet) filled in a crosswalk counts as missing in the *_clean columns.
caption_crosswalk <- "Cleaned fields use the filled disambiguation crosswalks; unfilled terms count as missing."
caption_district  <- "District: \"_\" (left blank on purpose), \"At Large\", and blanks from at-large states and territories count as recorded."


####COVERAGE####
#Number of Employees
employees <- registers %>% count(year, name = "n_employees")
p_employees <- ggplot(employees, aes(year, n_employees)) +
  geom_line(colour = register_colors[1], linewidth = line_width) +
  geom_point(colour = register_colors[1], size = point_size) +
  scale_x_year() +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Number of employees listed in each register", subtitle = "Log scale", x = NULL, y = "Employees")
save_plot(p_employees, "employees_by_year")
save_table(employees, "employees_by_year")

#Proportion non-missing in
  ##Appointment
  ##Birth
  ##Employed
  ##Congressional district
  ##Position
  ##Salary (direct and inferred)
coverage <- registers %>%
  group_by(year) %>%
  summarise(`State appointed`      = mean(!is.na(state_appointed_clean)),
            `State born`           = mean(!is.na(state_born_clean)),
            `State employed`       = mean(!is.na(state_employed_clean)),
            `Congressional district` = mean(cong_district_covered),
            `Position`             = mean(!is.na(position_clean)),
            `Salary, direct`       = mean(yearly_compensation_source %in% "direct"),
            `Salary, inferred`     = mean(yearly_compensation_source %in% "inferred"),
            .groups = "drop") %>%
  pivot_longer(-year, names_to = "field", values_to = "share") %>%
  #A state or district field that is 0% in a year was not recorded in that register at all, so show a gap instead of a drop to zero
  mutate(share = if_else(share == 0 & str_detect(field, "^State|^Congressional"), NA_real_, share),
         field = factor(field, levels = c("State appointed", "State born", "State employed", "Congressional district", "Position", "Salary, direct", "Salary, inferred")))
p_coverage <- ggplot(coverage, aes(year, share, colour = field)) +
  geom_line(linewidth = line_width) +
  geom_point(size = point_size) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_colour_register() +
  labs(title = "Share of employees with each field recorded", x = NULL, y = "Share of employees", caption = str_c("Gaps = field not recorded in that register. ", caption_crosswalk))
save_plot(p_coverage, "coverage_by_year")
save_table(coverage, "coverage_by_year")

#One coverage plot per element (salary shows its direct and inferred shares together)
coverage_elements <- list(`State appointed`        = "State appointed",
                          `State born`             = "State born",
                          `State employed`         = "State employed",
                          `Congressional district` = "Congressional district",
                          `Position`               = "Position",
                          `Salary`                 = c("Salary, direct", "Salary, inferred"))
iwalk(coverage_elements, function(fields, element) {
  d <- filter(coverage, field %in% fields)
  p <- ggplot(d, aes(year, share, colour = field)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    scale_x_year() +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    scale_colour_manual(values = set_names(register_colors[match(fields, levels(coverage$field))], fields)) +
    labs(title = str_c("Share of employees with ", str_to_lower(element), " recorded"), x = NULL, y = "Share of employees",
         caption = str_c("Gaps = field not recorded in that register. ", caption_crosswalk,
                         if (element == "Congressional district") str_c("\n", caption_district) else "")) +
    if (length(fields) == 1) theme(legend.position = "none")
  slug <- str_replace_all(str_to_lower(element), "[^a-z]+", "_")
  save_plot(p, str_c("coverage_", slug, "_by_year"))
})


####BIRTHPLACE HEATMAP####
#Animated map: share of employees (with a recorded birthplace) born in each of the 48 states, by register year.
#The DC and "Other" dots sit off the Atlantic coast and change colour and size with their share (same colour scale as the states,
#size proportional to the share). "Other" is everything that is not one of the 48 states or DC: foreign countries, territories
#and possessions, Alaska and Hawaii, at sea, etc.
states_48 <- setdiff(state.name, c("Alaska", "Hawaii"))
dc_names  <- c("DC", "District of Columbia", "Washington DC")
birth_share <- registers %>%
  filter(!is.na(state_born_clean)) %>%
  mutate(place = case_when(state_born_clean %in% states_48 ~ state_born_clean,
                           state_born_clean %in% dc_names  ~ "DC",
                           TRUE                            ~ "Other")) %>%
  count(year, place, name = "n") %>%
  complete(year, place = c(states_48, "DC", "Other"), fill = list(n = 0L)) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
save_table(birth_share, "state_born_share_by_year")

map_share <- map_data("state") %>%
  filter(region != "district of columbia") %>%
  mutate(place = str_to_title(region)) %>%
  inner_join(filter(birth_share, place %in% states_48), by = "place", relationship = "many-to-many")

dc_other_dots <- birth_share %>%
  filter(place %in% c("DC", "Other")) %>%
  mutate(lon = -70, lat = if_else(place == "DC", 36.5, 32.5),
         label = str_c(place, " ", percent(share, accuracy = 0.1)))

#Colour scale tops out at the 99th percentile of the state shares so one big state does not wash out the rest
share_cap <- quantile(filter(birth_share, place %in% states_48)$share, 0.99)
p_birth <- ggplot() +
  geom_polygon(data = map_share, aes(long, lat, group = group, fill = share), colour = surface_colour, linewidth = 0.2) +
  geom_point(data = dc_other_dots, aes(lon, lat, fill = share, size = share), shape = 21, colour = ink_secondary, stroke = 0.6) +
  geom_text(data = dc_other_dots, aes(lon + 1.8, lat, label = label), hjust = 0, size = 3.4, colour = ink_secondary) +
  scale_fill_gradient(low = "#f7fbff", high = register_colors[1], limits = c(0, share_cap), oob = squish,
                      labels = label_percent(), name = "Share born") +
  scale_size_area(max_size = 14, limits = c(0, max(dc_other_dots$share)), guide = "none") +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5, xlim = c(-125, -58)) +
  labs(title = "Where employees were born: {current_frame}",
       subtitle = "Share of employees with a recorded birthplace, by state (DC and Other shown as dots)",
       caption = str_wrap(str_c("Dot size and colour = share. Other = foreign countries, territories, Alaska/Hawaii, etc. ", caption_crosswalk), 130)) +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major = element_blank(),
        legend.key.width = unit(1.6, "cm"), legend.title = element_text(colour = ink_secondary)) +
  transition_manual(year)
birth_anim <- animate(p_birth, nframes = n_distinct(map_share$year), fps = 4, width = 1000, height = 650, res = 110,
                      renderer = gifski_renderer())
anim_save(file.path(figure_dir, "state_born_heatmap.gif"), birth_anim)


####COMPENSATION####
#Proportion with yearly salary, piecework, daily, weekly, monthly
#Classify the pay as written (rate type), independent of whether a yearly figure could be computed
pay_type_levels <- c("Yearly", "Weekly/monthly", "Daily", "Hourly", "Piecework", "Fees/commission", "Pay of rank/grade",
                     "Other", "See footnote", "No amount")
#Military ranks listed without a figure in the early registers: pay is implied by rank
#(a rank word anywhere in the title; plain "general" is left out so Attorney General, Consul General etc. stay as they are)
military_rank_pattern <- "captain|lieutenant|major|ensign|commander|colonel|brigadier|admiral|commodore|midshipm[ae]n|chaplain"
classify_pay <- function(x, position = NA_character_, year = NA_integer_) {
  s <- x %>%
    str_to_lower() %>%
    str_replace_all(c("½" = "", "¼" = "", "¾" = "", "⅓" = "", "⅔" = "", "⅛" = "")) %>%  #drop fraction signs
    str_remove_all("[[:space:]$,]")
  blank <- is.na(x) | s %in% c("", "_", "none")
  military_blank <- blank & year < 1907 & str_detect(str_to_lower(str_squish(position)), military_rank_pattern) %in% TRUE
  case_when(military_blank                                          ~ "Pay of rank/grade", #no figure printed; pay set by rank
            blank                                                   ~ "No amount",
            str_detect(s, "^[(][^)]*[)]$")                          ~ "See footnote",      #footnote marks like (1), (*): amount is in the footnote
            str_detect(s, "piece|pcwrk|pcwk")                       ~ "Piecework",
            str_detect(s, "perhour|[0-9]ph|cph")                    ~ "Hourly",
            str_detect(s, "perday|perdiem|[0-9]pd|cpd|p[.]d|^pd$")  ~ "Daily",
            str_detect(s, "perweek|[0-9]pw|permonth|[0-9]pm")       ~ "Weekly/monthly",
            str_detect(s, "fee|commission|cmsn|percent|centum")     ~ "Fees/commission",
            str_detect(s, "^pay|rank|armypay")                      ~ "Pay of rank/grade",  #PayRank, PayofRank, PayGrade, PayLt, ...
            str_detect(s, "^[0-9.]+(max|&sub[a-z]*|&qm&l|&qs&l|&quart|season|part)?$|perannum|peryear|p[.]a") ~ "Yearly",
            TRUE                                                    ~ "Other")  #per observation, honorary, several amounts, ...
}
pay_types <- registers %>%
  mutate(pay_type = factor(classify_pay(compensation, position_clean, year), levels = pay_type_levels)) %>%
  count(year, pay_type) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
p_pay_types <- ggplot(pay_types, aes(year, share, fill = pay_type)) +
  geom_col(width = 1, colour = surface_colour, linewidth = 0.2) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(values = c(register_colors[1:7], other_colour, footnote_colour, neutral_colour)) +
  labs(title = "How employees were paid", subtitle = "Pay basis as written in the register", x = NULL, y = "Share of employees")
save_plot(p_pay_types, "pay_type_by_year")
save_table(pay_types, "pay_type_by_year")

#Median salary, inferred and direct
#Real pay = nominal pay x (CPI in real_dollar_year / CPI in the register year), using the Officer & Williamson CPI
cpi <- read_csv(cpi_path, col_types = cols(year = col_integer(), cpi_1982_84_100 = col_double()))
cpi_base <- cpi$cpi_1982_84_100[cpi$year == real_dollar_year]
registers <- registers %>%
  left_join(mutate(cpi, deflator = cpi_base / cpi_1982_84_100), by = "year") %>%
  mutate(yearly_compensation_real = yearly_compensation_inferred * deflator)

median_pay <- registers %>%
  filter(!is.na(yearly_compensation_inferred)) %>%
  mutate(source = str_to_sentence(yearly_compensation_source)) %>%
  { bind_rows(., mutate(., source = "All")) } %>%
  group_by(year, source) %>%
  summarise(median_yearly = median(yearly_compensation_inferred), median_yearly_real = median(yearly_compensation_real),
            n = n(), .groups = "drop") %>%
  mutate(source = factor(source, levels = c("All", "Direct", "Inferred")))
p_median_pay <- ggplot(median_pay, aes(year, median_yearly, colour = source)) +
  geom_line(linewidth = line_width) +
  geom_point(size = point_size) +
  scale_x_year() +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_colour_register() +
  labs(title = "Median yearly compensation", subtitle = "Nominal dollars. Inferred = converted from day/hour/week/month rates.",
       x = NULL, y = "Median yearly compensation")
save_plot(p_median_pay, "median_yearly_compensation")
#US wage benchmarks (nominal, then deflated like the register pay). These are averages for production workers / unskilled labor,
#not medians, and are shown for scale rather than as a like-for-like comparison.
weekly_hours <- read_csv(hours_path, col_types = cols(.default = col_double()))
wages <- read_csv(wage_path, col_types = cols(.default = col_double())) %>%
  mutate(weekly_hours = approx(weekly_hours$year, weekly_hours$weekly_hours_manufacturing, xout = year, rule = 2)$y,
         hours_per_year = weekly_hours * wage_weeks_per_year,
         days_per_year  = pmin(wage_max_days_per_week, weekly_hours / wage_workday_hours) * wage_weeks_per_year) %>%
  left_join(mutate(cpi, deflator = cpi_base / cpi_1982_84_100), by = "year") %>%
  transmute(year,
            `US production workers` = production_worker_hourly_compensation_nominal * hours_per_year * deflator,
            `US unskilled labor`    = unskilled_labor_wage_index_1860_100 / 100 * unskilled_daily_wage_1860 * days_per_year * deflator) %>%
  pivot_longer(-year, names_to = "source", values_to = "median_yearly_real") %>%
  filter(!is.na(median_yearly_real))
series_levels <- c("All", "Direct", "Inferred", "US production workers", "US unskilled labor")
real_lines <- bind_rows(select(median_pay, year, source, median_yearly_real), wages) %>%
  mutate(source = factor(source, levels = series_levels),
         benchmark = source %in% c("US production workers", "US unskilled labor"))

p_median_real <- ggplot(real_lines, aes(year, median_yearly_real, colour = source, linetype = benchmark)) +
  geom_line(linewidth = line_width) +
  geom_point(data = filter(real_lines, !benchmark), size = point_size) +
  scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
  scale_x_year() +
  scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
  scale_colour_register() +
  labs(title = "Median yearly compensation, adjusted for inflation",
       subtitle = str_c("Constant ", real_dollar_year, " dollars. Inferred = converted from day/hour/week/month rates."),
       x = NULL, y = str_c("Median yearly compensation (", real_dollar_year, " dollars)"),
       caption = str_wrap(str_c("Deflated with the Officer & Williamson CPI (MeasuringWorth). Dashed lines are US wage averages (Officer & Williamson), ",
                                "not medians. Yearly = pay rate x actual hours: manufacturing weekly hours (EH.Net/Whaples) x ", wage_weeks_per_year,
                                " weeks; unskilled uses days = weekly hours / ", wage_workday_hours, ", at most ", wage_max_days_per_week,
                                " a week. Hourly pay is rounded to cents in the source."), 110))
save_plot(p_median_real, "median_yearly_compensation_real")
save_table(median_pay, "median_yearly_compensation")
save_table(wages, "us_wage_benchmarks_real")

#Salary distribution: median with 25th-75th (darker) and 10th-90th (lighter) percentile bands, direct and inferred salaries separately.
#Years with fewer than min_obs_for_percentiles salaries in a group are left out so thin years do not draw noisy bands.
min_obs_for_percentiles <- 30
salary_distribution <- function(value_col, y_label, title_suffix) {
  dist <- registers %>%
    filter(!is.na(.data[[value_col]]), !is.na(yearly_compensation_source)) %>%
    mutate(source = str_to_sentence(yearly_compensation_source)) %>%
    group_by(year, source) %>%
    filter(n() >= min_obs_for_percentiles) %>%
    summarise(n = n(),
              p10 = quantile(.data[[value_col]], 0.10), p25 = quantile(.data[[value_col]], 0.25),
              median = median(.data[[value_col]]),
              p75 = quantile(.data[[value_col]], 0.75), p90 = quantile(.data[[value_col]], 0.90),
              .groups = "drop") %>%
    mutate(source = factor(source, levels = c("Direct", "Inferred")))
  #Label each band edge at the right-hand end of each ribbon
  edge_labels <- dist %>%
    group_by(source) %>%
    slice_max(year, n = 1) %>%
    ungroup() %>%
    pivot_longer(c(p10, p25, median, p75, p90), names_to = "quantile", values_to = "value") %>%
    mutate(label = recode(quantile, p10 = "10th", p25 = "25th", median = "Median", p75 = "75th", p90 = "90th"))
  plot <- ggplot(dist, aes(year, fill = source, colour = source)) +
    geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.22, colour = NA) +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.45, colour = NA) +
    geom_line(aes(y = median), linewidth = line_width) +
    geom_text_repel(data = edge_labels, aes(y = value, label = label), nudge_x = 3, direction = "y", hjust = 0,
                    size = 3.3, segment.size = 0.25, min.segment.length = 0, box.padding = 0.15, seed = 1, show.legend = FALSE) +
    scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.12))) +
    scale_y_continuous(labels = label_dollar(), limits = c(0, NA)) +
    scale_fill_register() +
    scale_colour_register() +
    labs(title = str_c("Distribution of yearly compensation", title_suffix),
         subtitle = "Line = median; dark band = 25th-75th percentile; light band = 10th-90th percentile",
         x = NULL, y = y_label,
         caption = str_wrap(str_c("Direct = stated yearly salary; inferred = converted from day/hour/week/month rates. Years with fewer than ",
                                  min_obs_for_percentiles, " salaries in a group are omitted."), 110))
  list(plot = plot, data = dist)
}
salary_real <- salary_distribution("yearly_compensation_real", str_c("Yearly compensation (", real_dollar_year, " dollars)"),
                                   str_c(", ", real_dollar_year, " dollars"))
salary_nominal <- salary_distribution("yearly_compensation_inferred", "Yearly compensation (nominal dollars)", ", nominal dollars")
save_plot(salary_real$plot, "salary_distribution_real")
save_plot(salary_nominal$plot, "salary_distribution_nominal")
save_table(salary_real$data, "salary_distribution_real")
save_table(salary_nominal$data, "salary_distribution_nominal")


####GEOGRAPHY####
#Classify cleaned place names. Anything not listed below is treated as a foreign country or region.
confederate_states <- c("Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "Alabama",
                        "Mississippi", "Louisiana", "Texas", "Arkansas", "Tennessee")
dc_values          <- c("DC", "Washington DC")
us_other_domestic  <- c("Dakota", "Dakota Territory", "Indian Territory", "Hawaii Territory", "Honolulu",
                        "United States", "United States of America", "America", "Philadelphia", "Brooklyn", "Chicago")
us_possessions     <- c("Philippine Islands", "Philippines", "Manila", "Manila Philippine Islands", "Puerto Rico", "Guam",
                        "Canal Zone", "Samoa", "Virgin Islands")
not_a_place        <- c("Field", "DC and Field", "DC and Cuba", "At Sea", "Sea", "On High Seas", "Atlantic Coast", "Pacific Coast",
                        "Atlantic Ocean", "Naval Station Guantanamo", "Naval Station Olongapo", "Naval Hospital Las Animas",
                        "Navy Yard Norfolk", "Navy Yard Brooklyn", "Navy Yd Puget Sound", "San Carlos Reservation")
classify_place <- function(x) {
  case_when(is.na(x) | x %in% not_a_place              ~ NA_character_,
            x %in% c(state.name, dc_values, us_other_domestic) ~ "US",
            x %in% us_possessions                      ~ "US territory or possession",
            TRUE                                       ~ "Foreign")
}

#Where born
#Political status of a birthplace in a given reference year: "State or DC", "Territory" or "Foreign".
#Named states are territories before their admission year; overseas possessions are territories from the year the US acquired them
#(before that they count as foreign); the old names Dakota, Dakota Territory, Indian Territory and Hawaii Territory are always territories.
#Places that were never organized territories (Maine, West Virginia and Kentucky were parts of other states; Vermont, Texas and
#California went straight to statehood) count as states throughout. Louisiana, Florida, Alaska and Hawaii count as foreign before the US acquired them.
statehood_year <- c(Tennessee = 1796, Ohio = 1803, Louisiana = 1812, Indiana = 1816, Mississippi = 1817, Illinois = 1818, Alabama = 1819,
                    Missouri = 1821, Arkansas = 1836, Michigan = 1837, Florida = 1845, Iowa = 1846, Wisconsin = 1848, Minnesota = 1858,
                    Oregon = 1859, Kansas = 1861, Nevada = 1864, Nebraska = 1867, Colorado = 1876, `North Dakota` = 1889,
                    `South Dakota` = 1889, Montana = 1889, Washington = 1889, Idaho = 1890, Wyoming = 1890, Utah = 1896,
                    Oklahoma = 1907, `New Mexico` = 1912, Arizona = 1912, Alaska = 1959, Hawaii = 1959, Honolulu = 1959)
acquired_state_year <- c(Louisiana = 1803, Florida = 1821, Alaska = 1867, Hawaii = 1898, Honolulu = 1898)
acquired_year   <- c(`Philippine Islands` = 1898, Philippines = 1898, Manila = 1898, `Manila Philippine Islands` = 1898, `Puerto Rico` = 1898,
                    Guam = 1898, Samoa = 1900, `Canal Zone` = 1904, `Virgin Islands` = 1917)
territory_names <- c("Dakota", "Dakota Territory", "Indian Territory", "Hawaii Territory")
place_status <- function(place, ref_year) {
  base <- classify_place(place)
  case_when(is.na(base)                    ~ NA_character_,
            place %in% territory_names     ~ "Territory",
            place %in% names(statehood_year) ~ case_when(ref_year < coalesce(unname(acquired_state_year[place]), -Inf) ~ "Foreign",
                                                         ref_year < unname(statehood_year[place])                   ~ "Territory",
                                                         TRUE                                                       ~ "State or DC"),
            place %in% names(acquired_year)  ~ if_else(ref_year >= unname(acquired_year[place]), "Territory", "Foreign"),
            base == "US"                   ~ "State or DC",
            TRUE                           ~ "Foreign")
}
#The register gives a birthplace but no birth year, so "25 years earlier" is a rough stand-in for when a typical employee was born.
years_before_birth <- 25

born <- registers %>%
  mutate(status_at_register = place_status(state_born_clean, year),
         status_25_years_before = place_status(state_born_clean, year - years_before_birth)) %>%
  filter(!is.na(status_at_register))
save_table(count(born, year, status_at_register, status_25_years_before), "birthplace_status_by_year")

  ##Foreign born v. Domestic, and born in territories
#Four shares of employees with a recorded birthplace. The territory lines include overseas holdings (once acquired), so they overlap
#with the overseas line; foreign-born excludes holdings that had already been acquired.
birth_origin <- born %>%
  mutate(overseas = state_born_clean %in% names(acquired_year) & status_at_register == "Territory") %>%
  group_by(year) %>%
  summarise(`Foreign-born`                               = mean(status_at_register == "Foreign"),
            `Born in an overseas holding`                 = mean(overseas),
            `Born in a territory (at time of register)`   = mean(status_at_register == "Territory"),
            `Born in a territory (25 years before)`       = mean(status_25_years_before == "Territory"),
            .groups = "drop") %>%
  pivot_longer(-year, names_to = "group", values_to = "share") %>%
  mutate(group = factor(group, levels = c("Foreign-born", "Born in an overseas holding",
                                          "Born in a territory (at time of register)", "Born in a territory (25 years before)")))
p_birth_origin <- ggplot(birth_origin, aes(year, share, colour = group)) +
  geom_line(linewidth = line_width) +
  geom_point(size = point_size) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), limits = c(0, NA)) +
  scale_colour_register() +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = "Employees born outside the states", x = NULL, y = "Share of recorded birthplaces",
       caption = str_wrap(str_c("Overseas holdings (Philippines, Puerto Rico, Guam, Samoa, Canal Zone, Virgin Islands) count as foreign until acquired and as territory afterwards. ",
                                "The territory lines include overseas holdings and any state between acquisition and admission. ",
                                "The register has no birth year, so 25 years before publication is a rough proxy for when employees were born. ",
                                caption_crosswalk), 110))
save_plot(p_birth_origin, "born_outside_states_by_year")
save_table(birth_origin, "born_outside_states_by_year")

  ##North v. Confederacy
north_south <- registers %>%
  filter(classify_place(state_born_clean) %in% "US") %>%
  mutate(region = if_else(state_born_clean %in% confederate_states, "Confederate states", "Rest of the US")) %>%
  count(year, region) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = c("Confederate states", "Rest of the US")))
p_north_south <- ggplot(north_south, aes(year, share, fill = region)) +
  geom_col(width = 1, colour = surface_colour, linewidth = 0.2) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(values = register_colors[c(2, 1)]) +
  labs(title = "Birthplace of US-born employees: Confederate states vs. the rest", x = NULL,
       y = "Share of US-born employees", caption = str_c("Confederacy = the 11 seceding states. ", caption_crosswalk))
save_plot(p_north_south, "born_confederacy_vs_rest")
save_table(north_south, "born_confederacy_vs_rest")

  ##States
top_states <- registers %>%
  filter(classify_place(state_born_clean) %in% "US", !state_born_clean %in% dc_values) %>%
  count(state_born_clean, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(state_born_clean)
state_share <- registers %>%
  filter(!is.na(classify_place(state_born_clean))) %>%
  count(year, name = "n_all") %>%
  left_join(registers %>% filter(state_born_clean %in% top_states) %>% count(year, state_born_clean), by = "year") %>%
  mutate(share = n / n_all, state_born_clean = factor(state_born_clean, levels = top_states))
p_states <- ggplot(state_share, aes(year, share, colour = state_born_clean)) +
  geom_line(linewidth = line_width) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), limits = c(0, NA)) +
  scale_colour_register() +
  labs(title = "Most common birth states", subtitle = "The eight states that supply the most employees overall", x = NULL,
       y = "Share of employees with a recorded birthplace", caption = caption_crosswalk)
save_plot(p_states, "top_birth_states_by_year")
save_table(state_share, "top_birth_states_by_year")

#Where Employed
  ##DC V. Not
employed <- registers %>%
  filter(!is.na(classify_place(state_employed_clean))) %>%
  mutate(place = if_else(state_employed_clean %in% dc_values, "Washington, DC", "Elsewhere")) %>%
  count(year, place) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(place = factor(place, levels = c("Washington, DC", "Elsewhere")))
p_employed <- ggplot(employed, aes(year, share, fill = place)) +
  geom_col(width = 1, colour = surface_colour, linewidth = 0.2) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(values = register_colors[c(1, 2)]) +
  labs(title = "Where employees worked: Washington, DC vs. elsewhere", x = NULL,
       y = "Share of employees with a recorded work state",
       caption = str_c("Work state is only recorded through 1921. ", caption_crosswalk))
save_plot(p_employed, "employed_dc_vs_elsewhere")
save_table(employed, "employed_dc_vs_elsewhere")

  ##DC V. Not, excluding War and Navy
#War and Navy employ large field forces (arsenals, navy yards, Army and Navy officers) that are mostly outside DC, and some War rows have no recorded
#work state in some years (e.g. 1865). This repeats the chart above without them.
#The department field is raw text, so War and Navy are matched by pattern: spelling variants, the 1907-1921 abbreviations (War, USA, USN, USMCorps)
#and the 1905 field-force labels (e.g. "Quartermaster's Department at Large, Philippine Division").
#Not counted as War or Navy: the State, War and Navy Building superintendent's staff and the WWI boards (War Trade Board, War Finance Corporation).
shared_or_wartime_boards <- "state,? war,? (and|&) nav|sw&n|state ?war ?& ?nav|superintendent of state|^war ?(trade|finance|fin ?corp|industries|labor|risk)"
war_departments          <- "^war|department of (the )?war|army|^usa$|^quarter ?master'?s? department|^subsistence department|^engineer department|^ordnance department"
navy_departments         <- "^nav|navy|^usn$|usmcorps|marine corps"
is_war_or_navy <- function(x) {
  !is.na(x) & !str_detect(x, regex(shared_or_wartime_boards, ignore_case = TRUE)) &
    (str_detect(x, regex(war_departments, ignore_case = TRUE)) | str_detect(x, regex(navy_departments, ignore_case = TRUE)))
}
employed_excl_war_navy <- registers %>%
  filter(!is_war_or_navy(department), !is.na(classify_place(state_employed_clean))) %>%
  mutate(place = if_else(state_employed_clean %in% dc_values, "Washington, DC", "Elsewhere")) %>%
  count(year, place) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(place = factor(place, levels = c("Washington, DC", "Elsewhere")))
p_employed_excl_war_navy <- ggplot(employed_excl_war_navy, aes(year, share, fill = place)) +
  geom_col(width = 1, colour = surface_colour, linewidth = 0.2) +
  scale_x_year() +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(values = register_colors[c(1, 2)]) +
  labs(title = "Where employees worked: Washington, DC vs. elsewhere, excluding War and Navy", x = NULL,
       y = "Share of employees with a recorded work state",
       caption = str_c("Work state is only recorded through 1921. War and Navy are matched by department name (see script).
", caption_crosswalk))
save_plot(p_employed_excl_war_navy, "employed_dc_vs_elsewhere_excl_war_navy")
save_table(employed_excl_war_navy, "employed_dc_vs_elsewhere_excl_war_navy")

  ##Heatmap
#Animated map: share of employees (with a recorded work place) working in each of the 48 states, by register year. Built like the birthplace heatmap above.
#Same universe as the DC chart: rows whose work place is blank or not a place (Field, At Sea, ...) are dropped. DC and "Other" are dots off the Atlantic coast;
#"Other" is everything that is not one of the 48 states or DC: foreign countries, territories and possessions, Alaska and Hawaii, etc.
#Shares are of all employees with a recorded work place, so DC's large share leaves the state colours lighter in DC-heavy years.
employed_map_share <- registers %>%
  filter(!is.na(classify_place(state_employed_clean))) %>%
  mutate(place = case_when(state_employed_clean %in% states_48 ~ state_employed_clean,
                           state_employed_clean %in% c(dc_names, dc_values) ~ "DC",
                           TRUE                                ~ "Other")) %>%
  count(year, place, name = "n") %>%
  complete(year, place = c(states_48, "DC", "Other"), fill = list(n = 0L)) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
save_table(employed_map_share, "state_employed_share_by_year")

employed_map_polygons <- map_data("state") %>%
  filter(region != "district of columbia") %>%
  mutate(place = str_to_title(region)) %>%
  inner_join(filter(employed_map_share, place %in% states_48), by = "place", relationship = "many-to-many")

employed_dots <- employed_map_share %>%
  filter(place %in% c("DC", "Other")) %>%
  mutate(lon = -70, lat = if_else(place == "DC", 36.5, 32.5),
         label = str_c(place, " ", percent(share, accuracy = 0.1)))

#Colour scale tops out at the 99th percentile of the state shares so one big state does not wash out the rest
employed_share_cap <- quantile(filter(employed_map_share, place %in% states_48)$share, 0.99)
p_employed_map <- ggplot() +
  geom_polygon(data = employed_map_polygons, aes(long, lat, group = group, fill = share), colour = surface_colour, linewidth = 0.2) +
  geom_point(data = employed_dots, aes(lon, lat, fill = share, size = share), shape = 21, colour = ink_secondary, stroke = 0.6) +
  geom_text(data = employed_dots, aes(lon + 1.8, lat, label = label), hjust = 0, size = 3.4, colour = ink_secondary) +
  scale_fill_gradient(low = "#fff5eb", high = register_colors[2], limits = c(0, employed_share_cap), oob = squish,
                      labels = label_percent(), name = "Share employed") +
  scale_size_area(max_size = 14, limits = c(0, max(employed_dots$share)), guide = "none") +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5, xlim = c(-125, -58)) +
  labs(title = "Where employees worked: {current_frame}",
       subtitle = "Share of employees with a recorded work place, by state (DC and Other shown as dots)",
       caption = str_wrap(str_c("Dot size and colour = share. Other = foreign countries, territories, Alaska/Hawaii, etc. Work state is only recorded through 1921. ", caption_crosswalk), 130)) +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major = element_blank(),
        legend.key.width = unit(1.6, "cm"), legend.title = element_text(colour = ink_secondary)) +
  transition_manual(year)
employed_anim <- animate(p_employed_map, nframes = n_distinct(employed_map_polygons$year), fps = 4, width = 1000, height = 650, res = 110,
                         renderer = gifski_renderer())
anim_save(file.path(figure_dir, "state_employed_heatmap.gif"), employed_anim)
