## occupationRiskPack: Core API Functions
## All loaders, merge, and impute functions

# Suppress R CMD check NOTEs for data.frame column references used in dplyr pipelines
utils::globalVariables(c(
  "isco08_code", "isco88_code", "rti_score", "offs_score", 
  "skill_specificity", "ess_wave", "soc_code", "soc_2010_code", 
  "soc_2018_code", "n_digits", "description", "isco08_title",
  "major", "minor", "broad", "detailed"
))

# Internal helper: normalize ISCO codes to digits-only character
normalize_isco_codes <- function(codes) {
  as.character(gsub("[^0-9]", "", codes))
}

#' Load ISCO-08 baseline (427 4-digit occupation codes)
#'
#' Returns the canonical ISCO-08 occupations table used as the master list for merges.
#' @param path optional path to source_occup_risks.csv; if NULL uses inst/extdata
#' @return tibble keyed by `isco08_code`
#' @export
base_isco <- function(path = NULL) {
  message("Data source: International Standard Classification of Occupations (ISCO) 2008\n",
          "Check website: https://ilostat.ilo.org/methods/concepts-and-definitions/classification-occupation/\n",
          "This dataset contains 427 4-digit ISCO-08 occupation codes.")
  
  if (is.null(path)) {
    path <- system.file("extdata", "source_occup_risks.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_occup_risks.csv"),
        file.path("..", "inst", "extdata", "source_occup_risks.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_occup_risks.csv not found in inst/extdata/; please add file or pass path")
      path <- found
    }
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- make.names(names(df))
  if ("isco_08_4dg" %in% names(df)) names(df)[names(df) == "isco_08_4dg"] <- "isco08_code"
  if ("isco08" %in% names(df)) names(df)[names(df) == "isco08"] <- "isco08_code"
  if ("isco08_code" %in% names(df)) df$isco08_code <- as.character(df$isco08_code)
  if ("rti_mih19" %in% names(df)) names(df)[names(df) == "rti_mih19"] <- "rti_mihaylov_2019"
  if ("rti" %in% names(df)) names(df)[names(df) == "rti"] <- "rti_mihaylov_2019"
  if ("description" %in% names(df)) names(df)[names(df) == "description"] <- "isco08_title"
  return(dplyr::as_tibble(df))
}

#' Load ISCO-08 detailed structure (all levels with definitions)
#'
#' Returns the full ISCO-08 hierarchical structure including 1-, 2-, 3-, and 4-digit
#' codes with titles, definitions, task descriptions, and notes.
#' Use `n_digits` column to filter by level (e.g., `filter(n_digits == 4)` for 4-digit only).
#'
#' @param path optional path to CSV; if NULL uses inst/extdata/source_isco_structure.csv
#' @param digits optional integer (1, 2, 3, or 4) to filter to specific level; NULL returns all
#' @return tibble with columns: isco08_code, n_digits, isco08_title, definition, tasks_include, etc.
#' @examples
#' # Load all levels
#' # isco_detail <- base_isco_detail()
#'
#' # Load only 4-digit codes
#' # isco_4dig <- base_isco_detail(digits = 4)
#'
#' # Load 2-digit major groups
#' # isco_2dig <- base_isco_detail(digits = 2)
#' @export
base_isco_detail <- function(path = NULL, digits = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_isco_structure.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_isco_structure.csv"),
        file.path("..", "inst", "extdata", "source_isco_structure.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_isco_structure.csv not found in inst/extdata/")
      path <- found
    }
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  df$isco08_code <- as.character(df$isco08_code)
  
  # Filter by digits if specified

  if (!is.null(digits)) {
    if (!digits %in% 1:4) stop("digits must be 1, 2, 3, or 4")
    df <- df[df$n_digits == digits, ]
  }
  
  return(dplyr::as_tibble(df))
}

#' Load SOC 2018 baseline (867 detailed occupation codes)
#'
#' Returns the SOC 2018 occupations table with codes, titles, and definitions.
#' Use `level` parameter to filter by SOC hierarchy level.
#'
#' @section Important Note on Merging SOC Data:
#' When merging data from different sources using SOC 2018 codes, occupation titles 
#' may vary slightly between datasets (e.g., "Compliance Officers" vs 
#' "Compliance Officers, Except Agriculture, Construction..."). 
#' 
#' \strong{Always use `soc2018_code` as the merge key}, not occupation titles.
#' The numeric codes are standardized while titles may differ across sources.
#' 
#' \preformatted{
#' # Correct: merge by code
#' merged <- left_join(soc, other_data, by = "soc2018_code")
#' 
#' # Avoid: merging by title (may fail due to variations)
#' # merged <- left_join(soc, other_data, by = "soc2018_title")
#' }
#'
#' @param path optional path to CSV; if NULL uses inst/extdata/source_soc_2018.csv
#' @param level optional character ("Major", "Minor", "Broad", "Detailed") to filter; NULL returns all
#' @return tibble with columns: soc2018_code, n_digits, soc_group, soc2018_title, soc_definition
#' @examples
#' # Load all levels
#' # soc <- base_soc()
#'
#' # Load only detailed codes (6-digit)
#' # soc_detailed <- base_soc(level = "Detailed")
#' @export
base_soc <- function(path = NULL, level = NULL) {
  message("Data source: US Bureau of Labor Statistics, Standard Occupational Classification 2018\n",
          "Check website: https://www.bls.gov/soc/2018/\n",
          "This dataset contains 1447 SOC 2018 occupation codes across 4 hierarchical levels.")
  
  if (is.null(path)) {
    path <- system.file("extdata", "source_soc_2018.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_soc_2018.csv"),
        file.path("..", "inst", "extdata", "source_soc_2018.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_soc_2018.csv not found in inst/extdata/")
      path <- found
    }
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  # Rename to explicit SOC 2018 columns
  if ("soc_code" %in% names(df)) names(df)[names(df) == "soc_code"] <- "soc2018_code"
  if ("soc_title" %in% names(df)) names(df)[names(df) == "soc_title"] <- "soc2018_title"
  
  df$soc2018_code <- as.character(df$soc2018_code)
  
  # Filter by level if specified
  if (!is.null(level)) {
    valid_levels <- c("Major", "Minor", "Broad", "Detailed")
    if (!level %in% valid_levels) stop("level must be one of: ", paste(valid_levels, collapse = ", "))
    df <- df[df$soc_group == level, ]
  }
  
  return(dplyr::as_tibble(df))
}

#' Load SOC 2010 to 2018 crosswalk
#'
#' Returns the mapping between SOC 2010 and SOC 2018 codes.
#' Use this to update SOC 2010 codes (from ISCO-08 crosswalk) to SOC 2018.
#'
#' @param path optional path to CSV; if NULL uses inst/extdata/source_soc_crosswalk.csv
#' @return tibble with columns: soc2010_code, soc2010_title, soc2018_code, soc2018_title
#' @examples
#' # Load crosswalk
#' # cw <- crosswalk_soc_10_18()
#'
#' # Chain from ISCO-08 to SOC 2018:
#' # isco_to_soc2010 <- crosswalk_isco_soc(data, from = "isco")
#' # merged <- left_join(isco_to_soc2010, cw, by = c("soc" = "soc2010_code"))
#' @export
crosswalk_soc_10_18 <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_soc_crosswalk.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_soc_crosswalk.csv"),
        file.path("..", "inst", "extdata", "source_soc_crosswalk.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_soc_crosswalk.csv not found in inst/extdata/")
      path <- found
    }
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  df$soc2010_code <- as.character(df$soc2010_code)
  df$soc2018_code <- as.character(df$soc2018_code)
  
  return(dplyr::as_tibble(df))
}

#' Load RTI (Mihaylov 2019)
#'
#' Reads source_rti_mihaylov_2019.csv from inst/extdata.
#' Routine Task Intensity measures for 427 ISCO-08 occupations.
#' Source: Mihaylov & Tijdens (2019), Tinbergen Institute Discussion Paper 2019-035/V.
#' 
#' @param path optional path to CSV; if NULL tries inst/extdata/source_rti_mihaylov_2019.csv
#' @return tibble with `isco08_code` and `rti_mihaylov_2019`
#' @export
load_rti <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_rti_mihaylov_2019.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_rti_mihaylov_2019.csv"),
        file.path("..", "inst", "extdata", "source_rti_mihaylov_2019.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_rti_mihaylov_2019.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading RTI data for 427 ISCO-08 occupations")
  message("Source: Mihaylov, E., & Tijdens, K. G. (2019). 'Measuring the Routine and Non-Routine")
  message("        Task Content of 427 Four-Digit ISCO-08 Occupations.'")
  message("        Tinbergen Institute Discussion Paper 2019-035/V.")
  message("        Available at: https://ssrn.com/abstract=3389681")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- make.names(names(df))
  if ("isco_08_4dg" %in% names(df)) names(df)[names(df) == "isco_08_4dg"] <- "isco08_code"
  if ("isco08" %in% names(df)) names(df)[names(df) == "isco08"] <- "isco08_code"
  if ("description" %in% names(df)) names(df)[names(df) == "description"] <- "isco08_title"
  if ("rti" %in% names(df)) names(df)[names(df) == "rti"] <- "rti_mihaylov_2019"
  if ("isco08_code" %in% names(df)) df$isco08_code <- as.character(df$isco08_code)
  if ("rti_mihaylov_2019" %in% names(df)) df$rti_mihaylov_2019 <- as.numeric(df$rti_mihaylov_2019)
  return(dplyr::as_tibble(df))
}

#' Load greenness/brownness scores
#'
#' Reads source_greenness_brownness.csv from inst/extdata.
#' Greenness and brownness scores for ISCO-08 occupations.
#' Source: Cavallotti et al. (2025), "Green collars at the voting booth."
#' 
#' @param path optional path to CSV; if NULL tries inst/extdata/source_greenness_brownness.csv
#' @return tibble with `isco08_code`, `greenness`, `brownness`, `brown_dummy`
#' @export
load_brownness <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_greenness_brownness.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_greenness_brownness.csv"),
        file.path("..", "inst", "extdata", "source_greenness_brownness.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_greenness_brownness.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading greenness/brownness scores for ISCO-08 occupations")
  message("Source: Cavallotti, E., Colantone, I., Stanig, P., & Vona, F. (2025).")
  message("        'Green collars at the voting booth: Material interest and environmental voting.'")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- make.names(names(df))
  if ("isco_08_4dg" %in% names(df)) names(df)[names(df) == "isco_08_4dg"] <- "isco08_code"
  if ("isco08" %in% names(df)) names(df)[names(df) == "isco08"] <- "isco08_code"
  if ("greenness" %in% names(df)) df$greenness <- as.numeric(df$greenness)
  if ("brownness" %in% names(df)) df$brownness <- as.numeric(df$brownness)
  if ("brown_dummy" %in% names(df)) df$brown_dummy <- as.integer(df$brown_dummy)
  if ("isco08_code" %in% names(df)) df$isco08_code <- as.character(gsub("[^0-9]", "", df$isco08_code))
  return(dplyr::as_tibble(df))
}

#' Load Greenness / Brownness measures (Scholl, Turban, & Gal 2023)
#'
#' Reads `source_scholl_green_brown.csv` from inst/extdata.
#' Provides occupation-level measures of exposure to the green transition for
#' 119 ISCO-08 4-digit occupations. Greenness scores indicate the share of
#' "environmentally friendly" (task-based) job content crosswalked from 8-digit
#' SOC codes via Vona et al. (2018). Brownness scores indicate the share of
#' "polluting" (industry-based) job content crosswalked from 6-digit SOC codes.
#'
#' The dataset also includes "ambiguous grey" occupations (Table A4 in the
#' source paper) which exhibit both green and brown characteristics.
#'
#' @section Data Source:
#' Scholl, N., Turban, S., & Gal, P. N. (
#'   2023). "The green side of productivity: An international classification of
#'   green and brown occupations" (No. 33). OECD Productivity Working Papers,
#'   OECD Publishing.
#'
#' @section Classification:
#' International Standard Classification of Occupations (ISCO) 2008, 4-digit.
#'
#' @section Key Variables:
#' \describe{
#'   \item{isco08_code}{4-digit ISCO-08 occupation code}
#'   \item{greenness}{Share of green 6-digit SOC codes in the 4-digit ISCO code (employment weighted)}
#'   \item{greenness_uw}{Greenness share with uniform weighting across SOC codes}
#'   \item{emp_uw_brown_6d}{Share of employees in brown 6-digit SOC codes (uniform weighting)}
#'   \item{share_brown_6d}{Share of brown 6-digit SOC codes in the 4-digit ISCO code}
#' }
#'
#' @param path optional path to CSV; if NULL tries inst/extdata/source_scholl_green_brown.csv
#' @return tibble with greenness/brownness measures keyed by `isco08_code`
#' @seealso \code{\link{load_brownness}} for an alternative green/brown measure (Cavallotti et al.)
#' @export
load_scholl_green_brown <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_scholl_green_brown.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_scholl_green_brown.csv"),
        file.path("..", "inst", "extdata", "source_scholl_green_brown.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_scholl_green_brown.csv not found in inst/extdata/")
      path <- found
    }
  }

  message("Source: Scholl, N., Turban, S., & Gal, P. N. (2023). The green side of productivity: ",
    "An international classification of green and brown occupations (No. 33). OECD Publishing.\n",
    "Coding scheme: International Standard Classification of Occupations (ISCO) 2008.")

  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- make.names(names(df))
  if ("isco_08_4dg" %in% names(df)) names(df)[names(df) == "isco_08_4dg"] <- "isco08_code"
  if ("isco08" %in% names(df)) names(df)[names(df) == "isco08"] <- "isco08_code"
  if ("isco08_code" %in% names(df)) df$isco08_code <- as.character(df$isco08_code)
  if ("greenness" %in% names(df)) df$greenness <- as.numeric(df$greenness)
  if ("greenness_uw" %in% names(df)) df$greenness_uw <- as.numeric(df$greenness_uw)
  if ("emp_uw_brown_6d" %in% names(df)) df$emp_uw_brown_6d <- as.numeric(df$emp_uw_brown_6d)
  if ("share_brown_6d" %in% names(df)) df$share_brown_6d <- as.numeric(df$share_brown_6d)
  df <- dplyr::as_tibble(df)
  if ("source" %in% names(df)) df <- dplyr::select(df, -source)
  return(df)
}

#' Load RTI and Offshorability from Mahutga et al. (2018)
#'
#' Loads Routine Task Intensity (RTI) and Offshorability scores from Mahutga et al. (2018).
#' Contains 27 ISCO-88 2-digit occupation codes across 178 country-year combinations.
#'
#' RTI and Offshorability scores vary by country and year because labor market structures
#' differ across contexts. The raw data preserves this variation.
#'
#' @section Data Structure:
#' The raw data contains country-year specific scores. Country codes are 2-letter ISO codes
#' (uppercase). Years are 4-digit (e.g., 2007, 2010).
#' 
#' @section Aggregation Options:
#' The \code{aggregate} parameter controls how to handle country-year variation:
#' \itemize{
#'   \item \code{"none"} (default): Returns full data with country, year, ISCO-88 code, and scores.
#'   \item \code{"by_isco"}: Collapses to mean RTI/Offshorability per ISCO-88 code across all country-years.
#' }
#'
#' @section Important Note on Classification:
#' This data uses ISCO-88 2-digit codes (e.g., "11", "12"), not ISCO-08 4-digit codes.
#' To merge with ISCO-08 data, use \code{\link{crosswalk_isco_88_08}} to bridge
#' the classifications.
#' 
#' @section Example Workflow:
#' \preformatted{
#' library(dplyr)
#' 
#' # Get aggregated (simple) version
#' mahutga <- load_mahutga(aggregate = "by_isco")
#' 
#' # Bridge to ISCO-08
#' cw <- crosswalk_isco_88_08()
#' cw <- cw \%>\% mutate(isco88_2digit = substr(isco88_code, 1, 2))
#' merged <- left_join(cw, mahutga, by = c("isco88_2digit" = "isco88_code"))
#' 
#' # Join with ISCO-08 data
#' isco08 <- base_isco()
#' final <- left_join(isco08, 
#'                    merged \%>\% select(isco08_code, rti_score, offs_score) \%>\% distinct(),
#'                    by = "isco08_code")
#' }
#' 
#' @param aggregate Character. Aggregation level: "none" (default) or "by_isco".
#' @param years Integer vector. Filter to specific year(s). NULL = all years.
#' @param countries Character vector. Filter to specific country codes (e.g., c("DE", "FR")). NULL = all countries.
#' @param path optional path to CSV; if NULL uses inst/extdata/source_rti_ofs_mahutga.csv
#' @return tibble with RTI and Offshorability data. Columns depend on aggregation level.
#' @references
#' Mahutga, M. C., Curran, M., & Roberts, A. (2018). Job tasks and the comparative 
#' structure of income and employment: Routine task intensity and offshorability 
#' for the LIS. International Journal of Comparative Sociology, 59(2), 81-109.
#' @seealso \code{\link{crosswalk_isco_88_08}} for bridging ISCO-88 to ISCO-08
#' @export
load_mahutga <- function(aggregate = c("none", "by_isco"),
                         years = NULL,
                         countries = NULL,
                         path = NULL) {
  aggregate <- match.arg(aggregate)
  
  if (is.null(path)) {
    path <- system.file("extdata", "source_rti_ofs_mahutga.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_rti_ofs_mahutga.csv"),
        file.path("..", "inst", "extdata", "source_rti_ofs_mahutga.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_rti_ofs_mahutga.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading RTI and Offshorability data (27 ISCO-88 2-digit codes, 178 country-years)\n",
          "Source: Mahutga, M. C., Curran, M., & Roberts, A. (2018).\n",
          "        'Job tasks and the comparative structure of income and employment.'\n",
          "        International Journal of Comparative Sociology, 59(2), 81-109.\n",
          "NOTE: Uses ISCO-88 2-digit codes. Use crosswalk_isco_88_08() to bridge to ISCO-08.")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Parse dname into country and year (e.g., "at07" -> "AT", 2007; "at95" -> "AT", 1995)
  if ("dname" %in% names(df)) {
    df$country <- toupper(substr(df$dname, 1, 2))
    year_part <- as.integer(substr(df$dname, 3, 4))
    # Convert 2-digit year to 4-digit (assume 19xx for >= 50, 20xx for < 50)
    df$year <- as.integer(ifelse(year_part >= 50, 
                                  1900L + year_part, 
                                  2000L + year_part))
    df$dname <- NULL
  }
  
  # Standardize column names
  if ("isco88_code_2dg" %in% names(df)) names(df)[names(df) == "isco88_code_2dg"] <- "isco88_code"
  
  # Ensure proper column types
  df$isco88_code <- as.character(df$isco88_code)
  df$rti_score <- as.numeric(df$rti_score)
  df$offs_score <- as.numeric(df$offs_score)
  
  # Reorder columns
  df <- df[, c("country", "year", "isco88_code", "rti_score", "offs_score")]
  
  # Filter by years if specified
  if (!is.null(years)) {
    df <- df[df$year %in% years, ]
    message("Filtered to year(s): ", paste(years, collapse = ", "))
  }
  
  # Filter by countries if specified
  if (!is.null(countries)) {
    countries <- toupper(countries)
    available <- unique(df$country)
    missing <- setdiff(countries, available)
    if (length(missing) > 0) {
      warning("Countries not found in data: ", paste(missing, collapse = ", "))
    }
    df <- df[df$country %in% countries, ]
    message("Filtered to countries: ", paste(intersect(countries, available), collapse = ", "))
  }
  
  # Aggregate if requested
  if (aggregate == "by_isco") {
    df <- df |>
      dplyr::group_by(isco88_code) |>
      dplyr::summarize(
        rti_score = mean(rti_score, na.rm = TRUE),
        offs_score = mean(offs_score, na.rm = TRUE),
        .groups = "drop"
      )
    message("Aggregated to mean RTI/Offshorability per ISCO-88 code (across all country-years)")
  }
  
  return(dplyr::as_tibble(df))
}

#' Load offshorability index (Blinder 2007)
#'
#' Reads source_blinder_offshorability.csv from inst/extdata.
#' Data based on O*NET data 2006 version 10.0.
#' For details, see: Blinder, A. S. (2007). "How Many U.S. Jobs Might Be Offshorable?"
#' CEPS Working Paper No. 142, Princeton University.
#' 
#' @section Note on SOC Titles:
#' SOC occupation titles (soc2010_title) may vary slightly between different source
#' datasets. For reliable merging across datasets, use `soc2010_code` rather than
#' title strings.
#' 
#' @param path optional path to CSV; if NULL tries inst/extdata/source_blinder_offshorability.csv
#' @return tibble with `soc2010_code`, `soc2010_title`, and `offshorability_index` (range 25-100)
#' @export
load_offshorability <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_blinder_offshorability.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_blinder_offshorability.csv"),
        file.path("..", "inst", "extdata", "source_blinder_offshorability.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_blinder_offshorability.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading offshorability data based on O*NET data 2006 version 10.0")
  message("Source: Blinder, A. S. (2007). 'How Many U.S. Jobs Might Be Offshorable?'")
  message("        CEPS Working Paper No. 142, Princeton University, March 2007")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Standardize column names
  # Rename to explicit SOC 2010 columns (Blinder 2007 used SOC 2010)
  if ("soc_code" %in% names(df)) names(df)[names(df) == "soc_code"] <- "soc2010_code"
  if ("soc_title" %in% names(df)) names(df)[names(df) == "soc_title"] <- "soc2010_title"
  
  if ("soc2010_code" %in% names(df)) df$soc2010_code <- as.character(df$soc2010_code)
  if ("offshorability_index" %in% names(df)) df$offshorability_index <- as.numeric(df$offshorability_index)
  
  return(dplyr::as_tibble(df))
}

#' Load computerization risk (Frey & Osborne 2017)
#'
#' Reads source_frey_osborne_computerization.csv from inst/extdata.
#' Computerization risk estimates for 702 SOC 2010 occupations.
#' Source: Frey, C. B., & Osborne, M. A. (2017). "The future of employment: 
#' How susceptible are jobs to computerisation?" Technological Forecasting 
#' and Social Change, 114, 254-280.
#' 
#' @section Note on SOC Titles:
#' Occupation titles may differ slightly across datasets. When merging SOC data,
#' always use \code{soc2010_code} as the merge key, not titles.
#' 
#' @param path optional path to CSV; if NULL tries inst/extdata/source_frey_osborne_computerization.csv
#' @return tibble with `soc2010_code`, `soc2010_title`, and `computerization_risk` (range 0-1)
#' @export
load_computerization <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_frey_osborne_computerization.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_frey_osborne_computerization.csv"),
        file.path("..", "inst", "extdata", "source_frey_osborne_computerization.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_frey_osborne_computerization.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading computerization risk data for 702 SOC 2010 occupations")
  message("Source: Frey, C. B., & Osborne, M. A. (2017).")
  message("        'The future of employment: How susceptible are jobs to computerisation?'")
  message("        Technological Forecasting and Social Change, 114, 254-280.")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Standardize column names
  if ("soc2010_code" %in% names(df)) df$soc2010_code <- as.character(df$soc2010_code)
  if ("computerization_risk" %in% names(df)) df$computerization_risk <- as.numeric(df$computerization_risk)
  
  return(dplyr::as_tibble(df))
}

#' Load Occupational Prestige Scores (Hughes et al. 2024)
#'
#' Reads source_prestige_hughes.csv from inst/extdata.
#' This dataset provides occupational prestige ratings based on the Occupational Prestige 
#' Ratings (OPR) system, with linkages to SOC 2018, O*NET, Census, and GSS coding schemes.
#' 
#' @section Data Structure:
#' This dataset includes:
#' \itemize{
#'   \item \strong{OPR Classification}: OPR_code, OPR_title, OPR_prestige_score, 
#'         OPR_field_code, OPR_field_title, OPR_field_rating
#'   \item \strong{SOC 2018}: soc2018_code, soc2018_title
#'   \item \strong{Other Classifications}: ONET, Census 1980/2010, GSS ratings 1989/2012
#' }
#' 
#' @section Important Notes:
#' \strong{Multiple Entries per SOC Code:}
#' Some SOC 2018 codes have multiple entries because the OPR system provides 
#' more granular occupational distinctions. For example, SOC code 27-2012 
#' "Producers and Directors" includes separate ratings for "Talent Director" 
#' and "Technical Director and/or Manager". This reflects real prestige variation 
#' within official SOC categories.
#' 
#' \strong{Missing SOC Codes:}
#' Some rows have missing SOC 2018 codes (NA) because:
#' \itemize{
#'   \item The occupation doesn't map cleanly to SOC 2018
#'   \item It represents a custom occupational category used in the survey
#'   \item It's an aggregated field-level rating
#' }
#' 
#' @section Usage for Merging with SOC 2018:
#' To merge with SOC 2018 data, you'll need to handle multiple entries per code.
#' 
#' Option 1: Filter to non-missing SOC codes and aggregate
#' \preformatted{
#' library(dplyr)
#' prestige <- load_prestige() %>%
#'   filter(!is.na(soc2018_code)) %>%
#'   group_by(soc2018_code, soc2018_title) %>%
#'   summarize(
#'     prestige_mean = mean(OPR_prestige_score, na.rm = TRUE),
#'     prestige_sd = sd(OPR_prestige_score, na.rm = TRUE),
#'     n_opr_occupations = n(),
#'     .groups = "drop"
#'   )
#' 
#' # Then merge with SOC data
#' soc <- base_soc()
#' merged <- left_join(soc, prestige, by = "soc2018_code")
#' }
#' 
#' Option 2: Keep detailed data (many-to-many merge)
#' \preformatted{
#' prestige <- load_prestige() %>% filter(!is.na(soc2018_code))
#' merged <- left_join(soc, prestige, 
#'                     by = "soc2018_code", 
#'                     relationship = "many-to-many")
#' }
#' 
#' @section Note on SOC Titles:
#' Occupation titles (soc2018_title) may differ from official BLS titles.
#' When merging with other SOC 2018 data, always use \code{soc2018_code} as the 
#' merge key, not titles. See \code{\link{base_soc}} for details.
#' 
#' @section Citation:
#' Hughes, B. T., Srivastava, S., Leszko, M., & Condon, D. M. (2024). 
#' Occupational prestige: The status component of socioeconomic status. 
#' Collabra: Psychology, 10(1), 92882. 
#' https://doi.org/10.1525/collabra.92882
#'
#' @param path optional path to CSV; if NULL uses inst/extdata/source_prestige_hughes.csv
#' @return tibble with OPR classification, prestige scores, and SOC 2018 linkages
#' @seealso \code{\link{base_soc}} for SOC 2018 classification system
#' @export
load_prestige <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_prestige_hughes.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_prestige_hughes.csv"),
        file.path("..", "inst", "extdata", "source_prestige_hughes.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_prestige_hughes.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading Occupational Prestige Ratings (OPR) data\n",
          "Source: Hughes, B. T., Srivastava, S., Leszko, M., & Condon, D. M. (2024).\n",
          "        'Occupational prestige: The status component of socioeconomic status.'\n",
          "        Collabra: Psychology, 10(1), 92882.\n",
          "NOTE: This dataset includes multiple prestige ratings per SOC 2018 code.\n",
          "      Some rows have missing SOC codes. See ?load_prestige for details.")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Ensure proper column types
  if ("soc2018_code" %in% names(df)) df$soc2018_code <- as.character(df$soc2018_code)
  if ("OPR_prestige_score" %in% names(df)) df$OPR_prestige_score <- as.numeric(df$OPR_prestige_score)
  if ("OPR_code" %in% names(df)) df$OPR_code <- as.numeric(df$OPR_code)
  
  # Fix Excel date corruption in SOC codes (e.g., "21-Nov" should be "11-2021")
  # Excel converts codes like "11-2021" to date format "21-Nov"
  if ("soc2018_code" %in% names(df)) {
    # Detect corrupted codes (contain month abbreviations)
    corrupted <- grepl("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)", 
                       df$soc2018_code, ignore.case = TRUE)
    
    if (any(corrupted, na.rm = TRUE)) {
      # Convert corrupted codes back to proper SOC format
      # Pattern: "DD-Mon" -> "MM-DDDD" or "Mon-DD" -> "MM-DDDD"
      month_map <- c(Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, 
                     Jul=7, Aug=8, Sep=9, Oct=10, Nov=11, Dec=12)
      
      for (i in which(corrupted)) {
        code <- df$soc2018_code[i]
        if (is.na(code)) next
        
        # Match patterns like "21-Nov" or "Nov-21"
        if (grepl("^([0-9]{1,2})-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)$", code, ignore.case = TRUE)) {
          parts <- strsplit(code, "-")[[1]]
          day <- parts[1]
          month <- parts[2]
          month_num <- month_map[month]
          df$soc2018_code[i] <- sprintf("%02d-%s00", month_num, day)
        } else if (grepl("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-([0-9]{1,2})$", code, ignore.case = TRUE)) {
          parts <- strsplit(code, "-")[[1]]
          month <- parts[1]
          day <- parts[2]
          month_num <- month_map[month]
          df$soc2018_code[i] <- sprintf("%02d-%s00", month_num, day)
        }
      }
      
      message("NOTE: Fixed ", sum(corrupted, na.rm = TRUE), " Excel date-corrupted SOC codes (e.g., '21-Nov' -> '11-2100')")
    }
  }
  
  return(dplyr::as_tibble(df))
}

#' Load AI Occupational Exposure (AIOE) scores (Felten et al. 2021)
#'
#' Reads source_aioe_felten.csv from inst/extdata.
#' This dataset provides AI Occupational Exposure (AIOE) scores measuring
#' the degree to which occupations are exposed to advances in artificial intelligence.
#' 
#' @section Data Structure:
#' This dataset includes:
#' \itemize{
#'   \item \strong{soc2018_code}: SOC 2018 occupation code
#'   \item \strong{soc2018_title}: SOC 2018 occupation title
#'   \item \strong{AIOE}: AI Occupational Exposure score (higher = more exposed to AI)
#' }
#' 
#' @section About AIOE:
#' AIOE measures the degree to which occupations are exposed to AI advances.
#' The score is constructed by linking AI applications to occupational abilities
#' from O*NET. Higher scores indicate greater exposure to AI technologies.
#' This is distinct from automation risk - high AIOE occupations may be augmented
#' by AI rather than replaced.
#' 
#' @section Note on SOC Titles:
#' Occupation titles (soc2018_title) may differ slightly from official BLS titles.
#' When merging with other SOC 2018 data, always use \code{soc2018_code} as the 
#' merge key, not titles. See \code{\link{base_soc}} for details.
#' 
#' @section Citation:
#' Felten, E., Raj, M., & Seamans, R. (2021). Occupational, industry, and 
#' geographic exposure to artificial intelligence: A novel dataset and its 
#' potential uses. Strategic Management Journal, 42(12), 2195-2217.
#'
#' @param path optional path to CSV; if NULL uses inst/extdata/source_aioe_felten.csv
#' @return tibble with soc2018_code, soc2018_title, and AIOE score
#' @seealso \code{\link{base_soc}} for SOC 2018 classification system,
#'          \code{\link{load_computerization}} for automation/computerization risk
#' @export
load_aioe <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_aioe_felten.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_aioe_felten.csv"),
        file.path("..", "inst", "extdata", "source_aioe_felten.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_aioe_felten.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading AI Occupational Exposure (AIOE) data for SOC 2018 occupations\n",
          "Source: Felten, E., Raj, M., & Seamans, R. (2021).\n",
          "        'Occupational, industry, and geographic exposure to artificial intelligence:\n",
          "         A novel dataset and its potential uses.'\n",
          "        Strategic Management Journal, 42(12), 2195-2217.")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Ensure proper column types
  if ("soc2018_code" %in% names(df)) df$soc2018_code <- as.character(df$soc2018_code)
  if ("AIOE" %in% names(df)) df$AIOE <- as.numeric(df$AIOE)
  
  # Fix Excel date corruption in SOC codes (e.g., "Nov-11" should be "11-2011")
  if ("soc2018_code" %in% names(df)) {
    corrupted <- grepl("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)", 
                       df$soc2018_code, ignore.case = TRUE)
    
    if (any(corrupted, na.rm = TRUE)) {
      month_map <- c(Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, 
                     Jul=7, Aug=8, Sep=9, Oct=10, Nov=11, Dec=12)
      
      for (i in which(corrupted)) {
        code <- df$soc2018_code[i]
        if (is.na(code)) next
        
        if (grepl("^([0-9]{1,2})-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)$", code, ignore.case = TRUE)) {
          parts <- strsplit(code, "-")[[1]]
          day <- parts[1]
          month <- parts[2]
          month_num <- month_map[month]
          df$soc2018_code[i] <- sprintf("%02d-%s00", month_num, day)
        } else if (grepl("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-([0-9]{1,2})$", code, ignore.case = TRUE)) {
          parts <- strsplit(code, "-")[[1]]
          month <- parts[1]
          day <- parts[2]
          month_num <- month_map[month]
          df$soc2018_code[i] <- sprintf("%02d-%s00", month_num, day)
        }
      }
      
      message("NOTE: Fixed ", sum(corrupted, na.rm = TRUE), " Excel date-corrupted SOC codes")
    }
  }
  
  return(dplyr::as_tibble(df))
}

#' Load ONET Hazardous Conditions data
#'
#' Loads exposure to hazardous conditions scores from ONET Online.
#' Contains 894 occupations using ONET-SOC 2018 classification (8-digit codes like "47-2152.00").
#' 
#' The data includes:
#' - `hazard_context`: Score (0-100) indicating frequency of exposure to hazardous conditions.
#'   100 = Every day, 75 = Once a week or more, 50 = Once a month or more,
#'   25 = Once a year or more, 0 = Never.
#' - `job_zone`: Training/education level required (1-5). See Job Zone section below.
#'
#' @section Job Zone Categories:
#' Job Zone indicates the level of preparation needed for an occupation:
#' 
#' \strong{Job Zone 1 - Little or No Preparation Needed:}
#' Some occupations may require a high school diploma or GED. Little or no previous
#' work-related experience needed. Training ranges from a few days to a few months.
#' Examples: agricultural equipment operators, dishwashers, landscaping workers, baristas.
#' 
#' \strong{Job Zone 2 - Some Preparation Needed:}
#' Usually requires a high school diploma. Some previous work-related experience helpful.
#' Training from a few months to one year with experienced employees.
#' Examples: tellers, customer service representatives, security guards, dental lab technicians.
#' 
#' \strong{Job Zone 3 - Medium Preparation Needed:}
#' Requires vocational school, on-the-job experience, or associate's degree.
#' Previous work-related experience required (e.g., electrician apprenticeship).
#' Training usually 1-2 years with on-the-job experience.
#' Examples: electricians, court reporters, medical assistants, barbers.
#' 
#' \strong{Job Zone 4 - Considerable Preparation Needed:}
#' Most require a four-year bachelor's degree. Considerable work-related experience needed.
#' Several years of work-related experience, on-the-job training, and/or vocational training.
#' Examples: real estate brokers, database administrators, graphic designers, cost estimators.
#' 
#' \strong{Job Zone 5 - Extensive Preparation Needed:}
#' Most require graduate school (master's, Ph.D., M.D., or J.D.).
#' Extensive experience needed; many require more than five years.
#' Examples: pharmacists, lawyers, astronomers, surgeons, veterinarians.
#'
#' @section ONET-SOC vs SOC 2018 Codes:
#' ONET uses 8-digit codes (e.g., "47-2152.00", "47-2152.04") that are more granular
#' than the standard 6-digit SOC 2018 codes (e.g., "47-2152"). For example:
#' \itemize{
#'   \item SOC 2018: 47-2152 (Plumbers, Pipefitters, and Steamfitters)
#'   \item ONET: 47-2152.00 (Plumbers, Pipefitters, and Steamfitters) - hazard score 54
#'   \item ONET: 47-2152.04 (Solar Thermal Installers and Technicians) - hazard score 52
#' }
#'
#' Another example with multiple sub-codes:
#' \itemize{
#'   \item SOC 2018: 19-1029 (Biological Scientists, All Other)
#'   \item ONET: 19-1029.01 (Bioinformatics Scientists) - hazard score 11
#'   \item ONET: 19-1029.02 (Molecular and Cellular Biologists) - hazard score 51
#'   \item ONET: 19-1029.03 (Geneticists) - hazard score 50
#'   \item ONET: 19-1029.04 (Biologists) - hazard score 22
#' }
#'
#' To merge with standard SOC 2018 data, you can extract the first 7 characters
#' (6-digit code with hyphen) and aggregate:
#' \preformatted{
#' hazard <- load_hazard()
#' hazard$soc2018_code <- substr(hazard$onet_soc2018_code, 1, 7)
#' hazard_agg <- hazard |>
#'   dplyr::group_by(soc2018_code) |>
#'   dplyr::summarize(
#'     hazard_context = mean(hazard_context, na.rm = TRUE),
#'     job_zone = mean(job_zone, na.rm = TRUE),
#'     n_onet_codes = dplyr::n()
#'   )
#' }
#'
#' @section Note on SOC Titles:
#' SOC occupation titles (onet_soc2018_title) may vary slightly between different source
#' datasets. For reliable merging across datasets, use the code column rather than
#' title strings.
#'
#' @param path optional path to CSV; if NULL uses inst/extdata/source_hazard_cond.csv
#' @return tibble with onet_soc2018_code (8-digit), onet_soc2018_title, hazard_context (0-100),
#'         and job_zone (1-5)
#' @seealso \code{\link{base_soc}} for SOC 2018 classification system
#' @export
load_hazard <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_hazard_cond.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_hazard_cond.csv"),
        file.path("..", "inst", "extdata", "source_hazard_cond.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_hazard_cond.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading O*NET Hazardous Conditions data (894 occupations)\n",
          "Source: O*NET Online - Exposed to Hazardous Conditions (4.C.2.c.1.d)\n",
          "        https://www.onetonline.org/find/descriptor/result/4.C.2.c.1.d\n",
          "Note: Uses O*NET-SOC 2018 8-digit codes (more granular than standard SOC 2018 6-digit codes)")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Ensure proper column types
  if ("onet_soc2018_code" %in% names(df)) df$onet_soc2018_code <- as.character(df$onet_soc2018_code)
  if ("hazard_context" %in% names(df)) df$hazard_context <- as.integer(df$hazard_context)
  if ("job_zone" %in% names(df)) df$job_zone <- as.integer(df$job_zone)
  
  return(dplyr::as_tibble(df))
}

#' Load Skill Specificity data (Pardos-Prado & Xena 2019)
#'
#' Loads skill specificity (SS) scores based on Iversen & Soskice (2001) methodology,
#' as used in Pardos-Prado & Xena (2019). Contains 492 ISCO-88 codes across 32 countries
#' and 5 ESS waves (2002-2010).
#'
#' Higher SS values indicate more skill specificity: high task compartmentalization within
#' a major occupational group relative to its workforce share. The measure is calculated as:
#' (share of ISCO-88 unit groups in major cluster) / (share of workforce in that major group).
#'
#' @section Data Structure:
#' The raw data contains country-year specific SS values because labor market structure
#' varies across countries and time. ESS waves correspond to years:
#' \itemize{
#'   \item Wave 1: 2002
#'   \item Wave 2: 2004
#'   \item Wave 3: 2006
#'   \item Wave 4: 2008
#'   \item Wave 5: 2010
#' }
#'
#' @section Aggregation Options:
#' The \code{aggregate} parameter controls how to handle country-year variation:
#' \itemize{
#'   \item \code{"none"} (default): Returns full data with country, wave, ISCO-88 code, and SS.
#'     Use this when country-specific labor market structure matters for your analysis.
#'   \item \code{"by_isco"}: Collapses to mean SS per ISCO-88 code across all countries/waves.
#'     Use this for a single "typical" skill specificity value per occupation.
#'   \item \code{"by_isco_wave"}: Mean SS per ISCO-88 code within each wave.
#'     Use with \code{wave = 5} to get most recent (2010) cross-country averages.
#' }
#'
#' @section Note on ISCO-88 Codes:
#' This data uses ISCO-88 classification (not ISCO-08). To merge with ISCO-08 data,
#' use \code{\link{crosswalk_isco_88_08}} for bridging. Note that ISCO-88 to ISCO-08
#' mapping involves many-to-many relationships at the 4-digit level.
#'
#' @param aggregate Character. Aggregation level: "none" (default), "by_isco", or "by_isco_wave".
#' @param wave Integer vector. Filter to specific ESS wave(s) before aggregating (1-5). NULL = all waves.
#' @param countries Character vector. Filter to specific country codes (e.g., c("DE", "FR")). NULL = all countries.
#' @param path Optional path to CSV; if NULL uses inst/extdata/source_skill_specificity_cntry_year.csv
#' @return tibble with skill specificity data. Columns depend on aggregation level.
#' @references
#' Pardos-Prado, S., & Xena, C. (2019). Skill specificity and attitudes toward immigration.
#' American Journal of Political Science, 63(2), 286-304.
#'
#' Iversen, T., & Soskice, D. (2001). An asset theory of social policy preferences.
#' American Political Science Review, 95(4), 875-893.
#' @seealso \code{\link{crosswalk_isco_88_08}} for bridging ISCO-88 to ISCO-08
#' @export
load_skill_specificity <- function(aggregate = c("none", "by_isco", "by_isco_wave"),
                                    wave = NULL,
                                    countries = NULL,
                                    path = NULL) {
  aggregate <- match.arg(aggregate)
  
  if (is.null(path)) {
    path <- system.file("extdata", "source_skill_specificity_cntry_year.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_skill_specificity_cntry_year.csv"),
        file.path("..", "inst", "extdata", "source_skill_specificity_cntry_year.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_skill_specificity_cntry_year.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading Skill Specificity data (492 ISCO-88 codes, 32 countries, 5 ESS waves)\n",
          "Source: Pardos-Prado, S., & Xena, C. (2019).\n",
          "        'Skill specificity and attitudes toward immigration.'\n",
          "        American Journal of Political Science, 63(2), 286-304.\n",
          "Based on: Iversen, T., & Soskice, D. (2001) methodology.")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Standardize column names
  if ("country_ess" %in% names(df)) names(df)[names(df) == "country_ess"] <- "country"
  if ("essround" %in% names(df)) names(df)[names(df) == "essround"] <- "ess_wave"
  if ("skillsp" %in% names(df)) names(df)[names(df) == "skillsp"] <- "skill_specificity"
  
  # Ensure proper column types
  df$isco88_code <- as.character(df$isco88_code)
  df$country <- as.character(df$country)
  df$ess_wave <- as.integer(df$ess_wave)
  df$skill_specificity <- as.numeric(df$skill_specificity)
  
  # Filter by wave if specified

  if (!is.null(wave)) {
    if (!all(wave %in% 1:5)) {
      stop("wave must be integer(s) between 1 and 5")
    }
    df <- df[df$ess_wave %in% wave, ]
    message("Filtered to ESS wave(s): ", paste(wave, collapse = ", "))
  }
  
  # Filter by countries if specified
  if (!is.null(countries)) {
    countries <- toupper(countries)
    available <- unique(df$country)
    missing <- setdiff(countries, available)
    if (length(missing) > 0) {
      warning("Countries not found in data: ", paste(missing, collapse = ", "))
    }
    df <- df[df$country %in% countries, ]
    message("Filtered to countries: ", paste(intersect(countries, available), collapse = ", "))
  }
  
  # Aggregate if requested
  if (aggregate == "by_isco") {
    df <- df |>
      dplyr::group_by(isco88_code) |>
      dplyr::summarize(
        skill_specificity = mean(skill_specificity, na.rm = TRUE),
        .groups = "drop"
      )
    message("Aggregated to mean skill specificity per ISCO-88 code (across all countries/waves)")
  } else if (aggregate == "by_isco_wave") {
    df <- df |>
      dplyr::group_by(isco88_code, ess_wave) |>
      dplyr::summarize(
        skill_specificity = mean(skill_specificity, na.rm = TRUE),
        .groups = "drop"
      )
    message("Aggregated to mean skill specificity per ISCO-88 code within each ESS wave")
  }
  
  return(dplyr::as_tibble(df))
}

#' Load Employment Projections (BLS 2024-2034)
#'
#' Loads U.S. Bureau of Labor Statistics (BLS) 2024-34 Employment Projections data.
#' Contains 832 SOC 2018 occupation codes with employment levels, projected changes, 
#' median wages, and education/training requirements.
#'
#' @section Data Contents:
#' \itemize{
#'   \item Employment levels (in thousands) for 2024 and 2034
#'   \item Employment change 2024-2034 (absolute and percent)
#'   \item Annual average employment change
#'   \item Median annual wage (2024). See note below about top-coded values.
#'   \item Typical entry-level education
#'   \item Work experience requirements
#'   \item Typical on-the-job training
#' }
#'
#' @section Note on Median Wage:
#' The \code{median_annual_wage_2024} column has two special cases:
#' \itemize{
#'   \item \strong{NA values}: 6 occupations have missing wage data.
#'   \item \strong{Top-coded at $239,200}: BLS top-codes wages at this threshold.
#'     Occupations with wages equal to 239200 have actual median wages \emph{at or above}
#'     this value. Real-world wages for these occupations (e.g., physicians, executives)
#'     may be substantially higher.
#' }
#'
#' @section Important Note on Merging SOC Data:
#' When merging data from different sources using SOC 2018 codes, occupation titles 
#' may vary slightly between datasets. \strong{Always use `soc2018_code` as the 
#' merge key}, not occupation titles.
#'
#' @param path Optional path to CSV; if NULL uses inst/extdata/source_employment_projections.csv
#' @return tibble with 832 rows and 14 columns keyed by `soc2018_code`
#' @references
#' U.S. Bureau of Labor Statistics. 2024-34 Employment Projections.
#' \url{https://data.bls.gov/projections/occupationProj}
#' @seealso \code{\link{base_soc}} for SOC 2018 classification details
#' @export
load_employment_projections <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "source_employment_projections.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_employment_projections.csv"),
        file.path("..", "inst", "extdata", "source_employment_projections.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_employment_projections.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  message("Loading BLS 2024-34 Employment Projections\n",
          "Source: U.S. Bureau of Labor Statistics (BLS)\n",
          "Data: https://data.bls.gov/projections/occupationProj\n",
          "Contains 832 SOC 2018 codes. Note: median_annual_wage_2024 is top-coded at $239,200.")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Ensure soc2018_code is character

  df$soc2018_code <- as.character(df$soc2018_code)
  
  # Ensure numeric columns are numeric
  numeric_cols <- c("employment_2024_thousands", "employment_2034_thousands",
                    "employment_change_24to34_thousands", "employment_change_24to34_percent",
                    "employment_change_24to34_thousands_annual_average", "median_annual_wage_2024")
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  # Ensure code columns are integer
  code_cols <- c("education_code", "work_exp_code", "ojt_code")
  for (col in code_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.integer(df[[col]])
    }
  }
  
  return(dplyr::as_tibble(df))
}

#' Impute missing values by ISCO hierarchy
#'
#' Imputes missing values in a variable by averaging from higher-level ISCO groups (3-digit, then 2-digit).
#' @param data A data.frame with ISCO codes and a variable to impute
#' @param var Character name of the variable to impute
#' @param new_var Character name for the imputed column (default: var_with_imputed)
#' @param isco_key Character name of the ISCO code column (default: "isco08_code")
#' @return tibble with imputed values and impute level indicator
#' @export
impute_by_isco <- function(data, var, new_var = NULL, isco_key = "isco08_code") {
  if (is.null(new_var)) new_var <- paste0(var, "_with_imputed")
  level_col <- paste0(new_var, "_impute_level")
  
  data <- dplyr::as_tibble(data)
  data[[isco_key]] <- as.character(data[[isco_key]])
  
  # Create hierarchy columns
  data$isco_3dg <- substr(data[[isco_key]], 1, 3)
  data$isco_2dg <- substr(data[[isco_key]], 1, 2)
  
  # Calculate means at each level
  mean_3dg <- stats::aggregate(data[[var]], by = list(isco_3dg = data$isco_3dg), FUN = mean, na.rm = TRUE)
  names(mean_3dg)[2] <- "mean_3dg"
  mean_2dg <- stats::aggregate(data[[var]], by = list(isco_2dg = data$isco_2dg), FUN = mean, na.rm = TRUE)
  names(mean_2dg)[2] <- "mean_2dg"
  
  # Merge means back
  data <- merge(data, mean_3dg, by = "isco_3dg", all.x = TRUE)
  data <- merge(data, mean_2dg, by = "isco_2dg", all.x = TRUE)
  
  # Impute
  data[[new_var]] <- data[[var]]
  data[[level_col]] <- ifelse(is.na(data[[var]]), NA_integer_, 0L)
  
  # Fill from 3dg
  na_mask <- is.na(data[[new_var]])
  data[[new_var]][na_mask] <- data$mean_3dg[na_mask]
  data[[level_col]][na_mask & !is.na(data$mean_3dg)] <- 3L
  
  # Fill from 2dg
  na_mask <- is.na(data[[new_var]])
  data[[new_var]][na_mask] <- data$mean_2dg[na_mask]
  data[[level_col]][na_mask & !is.na(data$mean_2dg)] <- 2L
  
  # Clean up temp columns
  data$isco_3dg <- NULL
  data$isco_2dg <- NULL
  data$mean_3dg <- NULL
  data$mean_2dg <- NULL
  
  return(dplyr::as_tibble(data))
}

#' Impute missing values by SOC hierarchy
#'
#' Imputes missing values in a variable by averaging from higher-level SOC groups.
#' SOC codes follow the format XX-XXXX (6 digits total, e.g., "11-1011").
#' Imputation proceeds from more specific to more general levels:
#' 5-digit Broad Occupation (e.g., "11-101"), then 3-digit Minor Group (e.g., "11-1"),
#' then 2-digit Major Group (e.g., "11").
#'
#' @param data A data.frame with SOC codes and a variable to impute
#' @param var Character name of the variable to impute
#' @param new_var Character name for the imputed column (default: var_with_imputed)
#' @param soc_key Character name of the SOC code column (default: "soc2018_code")
#' @return tibble with imputed values and impute level indicator.
#'   The impute level column indicates: 0 = original value, 2 = imputed from 2-digit,
#'   3 = imputed from 3-digit, 5 = imputed from 5-digit.
#' @examples
#' \dontrun{
#' # Load employment projections
#' emp <- load_employment_projections()
#' 
#' # Impute missing wage values using SOC hierarchy
#' imputed <- impute_by_soc(emp, "median_annual_wage_2024")
#' 
#' # Check imputation levels
#' table(imputed$median_annual_wage_2024_with_imputed_impute_level)
#' }
#' @seealso \code{\link{impute_by_isco}} for ISCO-based imputation
#' @export
impute_by_soc <- function(data, var, new_var = NULL, soc_key = "soc2018_code") {
  if (is.null(new_var)) new_var <- paste0(var, "_with_imputed")
  level_col <- paste0(new_var, "_impute_level")
  
  data <- dplyr::as_tibble(data)
  data[[soc_key]] <- as.character(data[[soc_key]])
  
 # Create hierarchy columns
  # SOC format: XX-XXXX (e.g., "11-1011")
  # 2-digit Major Group: "11" (chars 1-2)
  # 3-digit Minor Group: "11-1" (chars 1-4, includes hyphen)
  # 5-digit Broad Occupation: "11-101" (chars 1-6)
  # 6-digit Detailed Occupation: "11-1011" (full code)
  data$soc_5dg <- substr(data[[soc_key]], 1, 6)  # e.g., "11-101"
  data$soc_3dg <- substr(data[[soc_key]], 1, 4)  # e.g., "11-1"
  data$soc_2dg <- substr(data[[soc_key]], 1, 2)  # e.g., "11"
  
  # Calculate means at each level
  mean_5dg <- stats::aggregate(data[[var]], by = list(soc_5dg = data$soc_5dg), FUN = mean, na.rm = TRUE)
  names(mean_5dg)[2] <- "mean_5dg"
  mean_3dg <- stats::aggregate(data[[var]], by = list(soc_3dg = data$soc_3dg), FUN = mean, na.rm = TRUE)
  names(mean_3dg)[2] <- "mean_3dg"
  mean_2dg <- stats::aggregate(data[[var]], by = list(soc_2dg = data$soc_2dg), FUN = mean, na.rm = TRUE)
  names(mean_2dg)[2] <- "mean_2dg"
  
  # Merge means back
  data <- merge(data, mean_5dg, by = "soc_5dg", all.x = TRUE)
  data <- merge(data, mean_3dg, by = "soc_3dg", all.x = TRUE)
  data <- merge(data, mean_2dg, by = "soc_2dg", all.x = TRUE)
  
  # Impute
  data[[new_var]] <- data[[var]]
  data[[level_col]] <- ifelse(is.na(data[[var]]), NA_integer_, 0L)
  
  # Fill from 5dg (Broad Occupation)
  na_mask <- is.na(data[[new_var]])
  data[[new_var]][na_mask] <- data$mean_5dg[na_mask]
  data[[level_col]][na_mask & !is.na(data$mean_5dg)] <- 5L
  
  # Fill from 3dg (Minor Group)
  na_mask <- is.na(data[[new_var]])
  data[[new_var]][na_mask] <- data$mean_3dg[na_mask]
  data[[level_col]][na_mask & !is.na(data$mean_3dg)] <- 3L
  
  # Fill from 2dg (Major Group)
  na_mask <- is.na(data[[new_var]])
  data[[new_var]][na_mask] <- data$mean_2dg[na_mask]
  data[[level_col]][na_mask & !is.na(data$mean_2dg)] <- 2L
  
  # Clean up temp columns
  data$soc_5dg <- NULL
  data$soc_3dg <- NULL
  data$soc_2dg <- NULL
  data$mean_5dg <- NULL
  data$mean_3dg <- NULL
  data$mean_2dg <- NULL
  
  return(dplyr::as_tibble(data))
}
