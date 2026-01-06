## Minimal summarize_risks implementation (lightweight, no extra deps)

#' Minimal summary of numeric risk variables by ISCO prefixes
#'
#' This minimal implementation avoids extra package dependencies and
#' computes simple per-variable summaries (n, n_non_missing, mean, sd,
#' median, missing_pct) overall and by requested ISCO prefix lengths.
#'
#' @param data A data.frame with ISCO codes and risk variables (required).
#' @param vars Character vector of variable names to summarize.
#' @param isco_key Character name of the ISCO code column.
#' @param digits Integer vector of ISCO prefix lengths to group by (e.g., c(1,2,3,4)).
#' @return A data.frame with summary statistics by ISCO prefix.
#' @importFrom stats median sd
#' @export
summarize_risks <- function(data,
                            vars = c("rti_mihaylov_2019", "greenness", "brownness"),
                            isco_key = "isco08_code",
                            digits = c(1,2,3,4)) {
  # Validate input
  if (missing(data) || is.null(data)) {
    stop("`data` is required. Please provide a data.frame with ISCO codes and risk variables.")
  }

  if (!isco_key %in% names(data)) stop(sprintf("ISCO key '%s' not found in data", isco_key))
  data[[isco_key]] <- as.character(data[[isco_key]])

  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))

  # coerce to numeric where possible
  for (v in vars) {
    if (!is.numeric(data[[v]])) suppressWarnings(data[[v]] <- as.numeric(data[[v]]))
  }

  summarize_vec <- function(vec) {
    n <- length(vec)
    non_missing <- sum(!is.na(vec))
    mean_v <- if (non_missing > 0) mean(vec, na.rm = TRUE) else NA_real_
    sd_v <- if (non_missing > 1) sd(vec, na.rm = TRUE) else NA_real_
    med_v <- if (non_missing > 0) median(vec, na.rm = TRUE) else NA_real_
    missing_pct <- 100 * (1 - non_missing / max(n, 1))
    c(n = n, n_non_missing = non_missing, mean = mean_v, sd = sd_v, median = med_v, missing_pct = missing_pct)
  }

  # overall (as data.frame with isco_digit = 'overall')
  overall <- do.call(rbind, lapply(vars, function(v) {
    vals <- summarize_vec(data[[v]])
    data.frame(isco_digit = 'overall', isco_prefix = NA_character_, variable = v,
               n = vals['n'], n_non_missing = vals['n_non_missing'],
               mean = vals['mean'], sd = vals['sd'], median = vals['median'],
               missing_pct = vals['missing_pct'], stringsAsFactors = FALSE)
  }))

  # by-digit combined into a single data.frame with column isco_digit
  by_digit_rows <- do.call(rbind, lapply(unique(digits), function(d) {
    if (!is.numeric(d) || d < 1) return(NULL)
    prefixes <- ifelse(nchar(data[[isco_key]]) >= d, substr(data[[isco_key]], 1, d), data[[isco_key]])
    split_data <- split(data, prefixes)
    df_list <- lapply(names(split_data), function(pref) {
      df <- split_data[[pref]]
      do.call(rbind, lapply(vars, function(v) {
        vals <- summarize_vec(df[[v]])
        data.frame(isco_digit = as.character(d), isco_prefix = pref, variable = v,
                   n = vals['n'], n_non_missing = vals['n_non_missing'],
                   mean = vals['mean'], sd = vals['sd'], median = vals['median'],
                   missing_pct = vals['missing_pct'], stringsAsFactors = FALSE)
      }))
    })
    do.call(rbind, df_list)
  }))

  # final result: rows for overall (isco_digit='overall') and all by-digit rows
  result_df <- rbind(overall, by_digit_rows)
  result_df
}
