## Analysis utility functions for occupation risk variables
#' Get top and bottom occupations by a risk variable
#'
#' Extract the top and bottom n occupations ranked by a numeric risk variable.
#' Optionally group results by ISCO prefix level (1-digit, 2-digit, etc.).
#' Returns occupations with their scores and percentile ranks for context.
#'
#' @param data A data.frame containing occupational data (expected to have
#'   an `isco08_code` column and occupation name/title).
#' @param var Character name of the numeric variable to rank by (e.g. "rti_mihaylov_2019").
#' @param n Integer number of top/bottom occupations to return (default 10).
#'   The result will have 2*n rows (n top + n bottom).
#' @param by_group Optional; if an integer (1, 2, 3, 4) then group results by ISCO
#'   prefix of that length. If NULL (default) return overall top/bottom.
#' @param isco_key Character name of the ISCO key column (default "isco08_code").
#'
#' @return A data.frame with columns:
#'   - `isco_prefix` (if by_group is set)
#'   - `isco08_code` occupation code
#'   - the risk variable value
#'   - `percentile` (rank percentile 0-100)
#'   - `rank_type` ('top' or 'bottom')
#'
#' @examples
#' # Get top 15 occupations by automation risk (RTI) overall
#' # merged <- merge_occup_risks()
#' # top_rti <- get_top_bottom_occupations(merged, var = "rti_mihaylov_2019", n = 15)
#'
#' # Get top/bottom 10 occupations by greenness, grouped by 1-digit ISCO major group
#' # by_major <- get_top_bottom_occupations(merged, var = "greenness", n = 10, by_group = 1)
#'
#' @export
get_top_bottom_occupations <- function(data, var, n = 10, by_group = NULL,
                                       isco_key = "isco08_code") {
  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  if (!var %in% names(data)) stop("Variable '", var, "' not found in data")
  if (!isco_key %in% names(data)) stop("ISCO key '", isco_key, "' not found in data")

  # Coerce var to numeric (safe coercion)
  if (!is.numeric(data[[var]])) {
    suppressWarnings(data[[var]] <- as.numeric(data[[var]]))
  }

  # Helper to rank within a group
  rank_and_extract <- function(df, var, n) {
    # drop NA rows for this variable
    df <- df[!is.na(df[[var]]), , drop = FALSE]
    if (nrow(df) == 0) return(data.frame())

    # compute percentiles
    df$percentile <- rank(df[[var]], na.last = "keep") / nrow(df) * 100
    df$percentile <- round(df$percentile, 1)

    # get top n
    top_idx <- order(df[[var]], decreasing = TRUE)[seq_len(min(n, nrow(df)))]
    top <- df[top_idx, , drop = FALSE]
    top$rank_type <- "top"

    # get bottom n
    bottom_idx <- order(df[[var]], decreasing = FALSE)[seq_len(min(n, nrow(df)))]
    bottom <- df[bottom_idx, , drop = FALSE]
    bottom$rank_type <- "bottom"

    rbind(top, bottom)
  }

  # Get rankings
  if (!is.null(by_group)) {
    if (!by_group %in% c(1, 2, 3, 4)) stop("by_group must be 1, 2, 3, or 4")
    data$isco_prefix <- substr(data[[isco_key]], 1, by_group)
    groups <- split(data, data$isco_prefix)
    result <- do.call(rbind, lapply(groups, rank_and_extract, var = var, n = n))
  } else {
    result <- rank_and_extract(data, var = var, n = n)
  }

  # Select and reorder columns
  if (!is.null(by_group)) {
    out_cols <- c("isco_prefix", isco_key, var, "percentile", "rank_type")
  } else {
    out_cols <- c(isco_key, var, "percentile", "rank_type")
  }
  
  # Keep only columns that exist
  out_cols <- out_cols[out_cols %in% names(result)]
  result <- result[, out_cols, drop = FALSE]

  rownames(result) <- NULL
  result
}

#' Lookup occupation titles and codes
#'
#' Simple utility to match ISCO-08 4-digit codes to occupation titles or vice versa.
#' Users specify which column to look up FROM (codes or titles), and the function
#' returns the matching counterpart.
#'
#' @param data A data.frame containing occupational data with ISCO codes and titles.
#' @param lookup_values Character vector of values to look up.
#' @param lookup_col Character name of the column to search IN (the source column).
#'   E.g., "isco08_code" to look up codes, or "occupation_title" to look up titles.
#' @param return_col Character name of the column to return (the target column).
#'   E.g., "occupation_title" when looking up by codes, or "isco08_code" when looking up by titles.
#' @param case_insensitive Logical; if TRUE (default), title lookups are case-insensitive.
#'   Ignored when lookup_col contains codes.
#'
#' @return A data.frame with columns `lookup_col` and `return_col`, showing matched pairs.
#'   If no matches found, returns empty data.frame.
#'
#' @examples
#' # Lookup titles for given codes
#' # df <- base_isco()
#' # lookup_isco(df, lookup_values = c("1111", "2110"), 
#' #             lookup_col = "isco08_code", return_col = "isco08_title")
#'
#' # Lookup codes for given titles (case-insensitive)
#' # lookup_isco(df, lookup_values = c("engineers", "architects"),
#' #             lookup_col = "occupation_title", return_col = "isco08_code")
#'
#' @export
lookup_isco <- function(data, lookup_values, lookup_col, return_col, case_insensitive = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame")
  if (missing(lookup_values)) stop("`lookup_values` must be provided")
  if (missing(lookup_col)) stop("`lookup_col` must be specified")
  if (missing(return_col)) stop("`return_col` must be specified")

  if (!lookup_col %in% names(data)) stop("Lookup column '", lookup_col, "' not found in data")
  if (!return_col %in% names(data)) stop("Return column '", return_col, "' not found in data")

  # Convert lookup_values to character
  lookup_values <- as.character(lookup_values)

  # Lookup with optional case-insensitivity
  if (case_insensitive) {
    values_lower <- tolower(lookup_values)
    data_lower <- tolower(as.character(data[[lookup_col]]))
    idx <- which(data_lower %in% values_lower)
  } else {
    idx <- which(as.character(data[[lookup_col]]) %in% lookup_values)
  }

  # Extract matching rows
  result <- data[idx, c(lookup_col, return_col), drop = FALSE]

  # Remove duplicates and reset rownames
  result <- result[!duplicated(result), , drop = FALSE]
  rownames(result) <- NULL
  result
}

