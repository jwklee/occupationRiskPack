## ----setup, include=FALSE-----------------------------------------------------
#library(occupationRiskPack)
library(dplyr)
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
# # Install from GitHub (when available)
# # devtools::install_github("jwklee/occupationRiskPack")
# 
# # Or install from CRAN (when available)
# # install.packages("occupationRiskPack")

## -----------------------------------------------------------------------------
# # Load the package
# library(occupationRiskPack)
# library(dplyr)

## -----------------------------------------------------------------------------
# # Load the baseline ISCO-08 table (427 4-digit codes)
# isco <- base_isco()
# head(isco)
# 
# # Load detailed ISCO-08 structure with definitions
# isco_detail <- base_isco_detail()
# head(isco_detail)
# 
# # Filter to specific level
# isco_2digit <- base_isco_detail(digits = 2)

## -----------------------------------------------------------------------------
# # Load SOC 2018 (all levels)
# soc <- base_soc()
# head(soc)
# 
# # Load only detailed (6-digit) occupations
# soc_detailed <- base_soc(level = "Detailed")

## -----------------------------------------------------------------------------
# # ISCO-88 to ISCO-08 crosswalk
# isco_xwalk <- crosswalk_isco_88_08()
# head(isco_xwalk)
# 
# # SOC 2010 to SOC 2018 crosswalk
# soc_xwalk <- crosswalk_soc_10_18()
# head(soc_xwalk)
# 
# # ISCO-08 to SOC 2010 crosswalk
# isco_soc_xwalk <- crosswalk_isco_08_soc_10()
# head(isco_soc_xwalk)

## -----------------------------------------------------------------------------
# # Load RTI data
# rti <- load_rti()
# head(rti)

## -----------------------------------------------------------------------------
# # Load AIOE data (Felten et al.)
# aioe <- load_aioe()
# head(aioe)

## -----------------------------------------------------------------------------
# # Load computerization risk (SOC 2010 codes)
# comp <- load_computerization()
# head(comp)

## -----------------------------------------------------------------------------
# # Load offshorability (Blinder 2007)
# offsh <- load_offshorability()
# head(offsh)

## -----------------------------------------------------------------------------
# # Load aggregated data (mean across countries/years)
# mahutga_agg <- load_mahutga(aggregate = "by_isco")
# head(mahutga_agg)
# 
# # Load granular country-year data
# mahutga_full <- load_mahutga(aggregate = "none")
# 
# # Filter to specific countries and years
# mahutga_subset <- load_mahutga(
#   aggregate = "none",
#   countries = c("DE", "NL"),
#   years = c(2010, 2013)
# )

## -----------------------------------------------------------------------------
# # Load aggregated skill specificity
# ss_agg <- load_skill_specificity(aggregate = "by_isco")
# head(ss_agg)
# 
# # Load country-wave specific data
# ss_full <- load_skill_specificity(aggregate = "none")
# 
# # Filter to specific ESS waves
# ss_wave5 <- load_skill_specificity(wave = 5)

## -----------------------------------------------------------------------------
# # Load greenness/brownness data
# brown <- load_brownness()
# head(brown)

## -----------------------------------------------------------------------------
# # Load Scholl/Turban/Gal greenness & brownness
# scholl <- load_scholl_green_brown()
# head(scholl)

## -----------------------------------------------------------------------------
# # Load prestige scores
# prestige <- load_prestige()
# head(prestige)

## -----------------------------------------------------------------------------
# # Load hazard data
# hazard <- load_hazard()
# head(hazard)

## -----------------------------------------------------------------------------
# hazard <- load_hazard()
# hazard$soc2018_code <- substr(hazard$onet_soc2018_code, 1, 7)
# hazard_agg <- hazard |>
#   dplyr::group_by(soc2018_code) |>
#   dplyr::summarize(
#     hazard_context = mean(hazard_context, na.rm = TRUE),
#     n_onet_codes = dplyr::n()
#   )

## -----------------------------------------------------------------------------
# # Load BLS projections
# emp <- load_employment_projections()
# head(emp)
# 
# # Explore employment change
# emp %>%
#   arrange(desc(employment_change_24to34_percent)) %>%
#   select(soc2018_code, soc2018_title,
#          employment_change_24to34_percent,
#          median_annual_wage_2024) %>%
#   head(10)

## -----------------------------------------------------------------------------
# # Combine data
# merged <- base_isco() %>%
#   left_join(load_rti(), by = "isco08_code") %>%
#   left_join(load_brownness(), by = "isco08_code")
# 
# # Get top 10 and bottom 10 by automation risk
# ranked <- get_top_bottom_occupations(
#   merged,
#   var = "rti_mihaylov_2019",
#   n = 10
# )
# print(ranked)

## -----------------------------------------------------------------------------
# # Top 3 within each 1-digit ISCO group
# ranked_by_group <- get_top_bottom_occupations(
#   merged,
#   var = "brownness",
#   n = 3,
#   by_group = 1
# )
# print(ranked_by_group)

## -----------------------------------------------------------------------------
# # Summarize by 2-digit ISCO groups
# summary_2dig <- summarize_risks(
#   merged,
#   vars = c("rti_mihaylov_2019", "brownness", "greenness"),
#   isco_key = "isco08_code"
# )
# head(summary_2dig)

## -----------------------------------------------------------------------------
# # Look up occupation titles
# titles <- lookup_isco(
#   base_isco(),
#   lookup_values = c("1111", "2111", "3111"),
#   lookup_col = "isco08_code",
#   return_col = "isco08_title"
# )
# print(titles)

## -----------------------------------------------------------------------------
# # Create sample data with brownness values
# data <- base_isco() %>%
#   left_join(load_brownness(), by = "isco08_code")
# 
# # Impute missing brownness values
# imputed <- impute_by_isco(
#   data,
#   var = "brownness",
#   new_var = "brownness_i",
#   isco_key = "isco08_code"
# )
# 
# # Check imputation levels
# table(imputed$brownness_i_impute_level)
# # 0 = original value, 2 = imputed from 2-digit, 3 = imputed from 3-digit

## -----------------------------------------------------------------------------
# # Impute missing wage values in employment projections
# emp <- load_employment_projections()
# emp_imputed <- impute_by_soc(
#   emp,
#   var = "median_annual_wage_2024",
#   new_var = "wage_i",
#   soc_key = "soc2018_code"
# )
# 
# # Check imputation levels
# table(emp_imputed$wage_i_impute_level)
# # 0 = original, 2 = from Major Group, 3 = from Minor Group, 5 = from Broad Occupation

## -----------------------------------------------------------------------------
# # Load and merge data
# data <- base_isco() %>%
#   left_join(load_rti(), by = "isco08_code")
# 
# # Add 1-digit major group
# data <- data %>%
#   mutate(major_group = substr(isco08_code, 1, 1))
# 
# # Summarize by major group
# data %>%
#   group_by(major_group) %>%
#   summarize(
#     mean_rti = mean(rti_mihaylov_2019, na.rm = TRUE),
#     sd_rti = sd(rti_mihaylov_2019, na.rm = TRUE),
#     n = n()
#   ) %>%
#   arrange(desc(mean_rti))

## -----------------------------------------------------------------------------
# # Load and merge
# data <- base_isco() %>%
#   left_join(load_rti(), by = "isco08_code") %>%
#   left_join(load_brownness(), by = "isco08_code")
# 
# # Calculate percentiles
# data <- data %>%
#   mutate(
#     rti_pctl = percent_rank(rti_mihaylov_2019) * 100,
#     brown_pctl = percent_rank(brownness) * 100
#   )
# 
# # Find double jeopardy occupations (top 30% on both)
# double_risk <- data %>%
#   filter(rti_pctl >= 70 & brown_pctl >= 70) %>%
#   arrange(desc(rti_pctl + brown_pctl)) %>%
#   select(isco08_code, isco08_title, rti_mihaylov_2019, brownness,
#          rti_pctl, brown_pctl)
# 
# head(double_risk, 15)

## -----------------------------------------------------------------------------
# # Load country-level data
# ss <- load_skill_specificity(aggregate = "none")
# 
# # Compare skill specificity across countries
# country_summary <- ss %>%
#   group_by(country) %>%
#   summarize(
#     mean_ss = mean(skill_specificity, na.rm = TRUE),
#     median_ss = median(skill_specificity, na.rm = TRUE),
#     n_occupations = n_distinct(isco88_code)
#   ) %>%
#   arrange(desc(mean_ss))
# 
# head(country_summary)

## -----------------------------------------------------------------------------
# # Load ISCO-88 data (skill specificity)
# ss <- load_skill_specificity(aggregate = "by_isco")
# 
# # Load crosswalk
# xwalk <- crosswalk_isco_88_08()
# 
# # Bridge to ISCO-08
# ss_isco08 <- ss %>%
#   left_join(xwalk, by = "isco88_code") %>%
#   # Handle many-to-many by taking mean
#   group_by(isco08_code) %>%
#   summarize(skill_specificity = mean(skill_specificity, na.rm = TRUE))
# 
# # Now merge with ISCO-08 data
# data <- base_isco() %>%
#   left_join(ss_isco08, by = "isco08_code")

