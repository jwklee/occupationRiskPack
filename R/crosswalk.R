## ISCO08 <-> SOC2010 crosswalk utilities
#' Load ISCO-08 to SOC 2010 Crosswalk
#'
#' Loads the crosswalk table between ISCO-08 (International Standard Classification 
#' of Occupations, 2008 revision) and SOC 2010 (US Standard Occupational Classification).
#'
#' @param path Path to the Excel crosswalk file; if NULL, uses the bundled file.
#' @return A data.frame with columns `isco08_code` and `soc2010_code`.
#' @export
#' @examples
#' \dontrun{
#' xwalk <- crosswalk_isco_08_soc_10()
#' head(xwalk)
#' }
crosswalk_isco_08_soc_10 <- function(path = NULL) {
  if (is.null(path)) {
    # Try installed package location first
    path <- system.file("extdata", "isco08toBLS.xlsx", package = "occupationRiskPack")
    # Fallback to local dev path
    if (path == "") {
      path <- file.path("inst", "extdata", "isco08toBLS.xlsx")
    }
  }
  if (!file.exists(path)) stop("Crosswalk file not found: ", path)
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required to read the crosswalk")
  # read first sheet; expect columns like ISCO08 and SOC2010 (names flexible)
  cw <- readxl::read_excel(path, sheet = 1)
  # normalise column names
  names(cw) <- tolower(gsub("\\s+", "_", names(cw)))
  # try to find isco and soc columns
  isco_col <- grep("isco", names(cw), value = TRUE)[1]
  soc_col <- grep("soc|bls", names(cw), value = TRUE)[1]
  if (is.null(isco_col) || is.null(soc_col)) stop("Could not find ISCO or SOC columns in crosswalk")
  cw <- data.frame(isco08_code = as.character(cw[[isco_col]]), soc2010_code = as.character(cw[[soc_col]]), stringsAsFactors = FALSE)
  # trim whitespace
  cw$isco08_code <- trimws(cw$isco08_code)
  cw$soc2010_code <- trimws(cw$soc2010_code)
  # Some cells may contain multiple SOC codes separated by commas or semicolons; split them
  cw_expanded <- do.call(rbind, lapply(seq_len(nrow(cw)), function(i) {
    isco <- cw$isco08_code[i]
    socs <- unlist(strsplit(cw$soc2010_code[i], "[,;\\/]", perl = TRUE))
    socs <- trimws(socs)
    data.frame(isco08_code = rep(isco, length(socs)), soc2010_code = socs, stringsAsFactors = FALSE)
  }))
  # drop empty entries
  cw_expanded <- cw_expanded[!(cw_expanded$isco08_code == "" & cw_expanded$soc2010_code == ""), , drop = FALSE]
  rownames(cw_expanded) <- NULL
  cw_expanded
}

#' Crosswalk between ISCO08 and SOC2010
#'
#' Map a data.frame from ISCO -> SOC or SOC -> ISCO. Many-to-many mappings are
#' expanded: each input row may produce multiple output rows when multiple
#' mapped codes exist. By default the function returns a new data.frame with
#' mapped rows; if `augment = TRUE` the function will add `mapped_code` and
#' `mapped_code_type` columns to the original rows and return an expanded
#' data.frame as well.
#'
#' @param data A data.frame containing either ISCO or SOC codes to map.
#'   If NULL, the raw crosswalk table is returned.
#' @param from Character, either "isco" or "soc" (direction of mapping).
#' @param code_col Character name of the code column in `data` to use. If NULL,
#'   defaults to `isco08_code` for ISCO and `soc2010` for SOC.
#' @param path Optional path to crosswalk Excel file (passed to loader).
#' @param augment Logical; if TRUE add `mapped_code` and `mapped_code_type` to
#'   the returned rows; if FALSE return only mapped rows with original cols plus
#'   mapping columns.
#' @return A data.frame with expanded mappings.
crosswalk_isco_soc <- function(data = NULL, from = c("isco", "soc"), code_col = NULL, path = NULL, augment = FALSE) {
  from <- match.arg(from)
  cw <- crosswalk_isco_08_soc_10(path = path)
  if (is.null(data)) return(cw)
  if (!is.data.frame(data)) stop("`data` must be a data.frame")

  if (is.null(code_col)) {
    code_col <- if (from == "isco") "isco08_code" else "soc2010_code"
  }
  if (!code_col %in% names(data)) stop("code_col not found in data: ", code_col)

  # Prepare mapping direction
  if (from == "isco") {
    # map ISCO -> SOC 2010
    # join by copying cw rows where cw$isco08_code == data[[code_col]]; expand many-to-many
    res <- do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
      code <- as.character(data[[code_col]][i])
      matches <- cw[cw$isco08_code == code, , drop = FALSE]
      if (nrow(matches) == 0) {
        # keep original row with NA mapped
        out <- data[i, , drop = FALSE]
        out$soc2010_code <- NA_character_
        out
      } else {
        do.call(rbind, lapply(seq_len(nrow(matches)), function(j) {
          out <- data[i, , drop = FALSE]
          out$soc2010_code <- matches$soc2010_code[j]
          out
        }))
      }
    }))
  } else {
    # map SOC 2010 -> ISCO
    res <- do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
      code <- as.character(data[[code_col]][i])
      matches <- cw[cw$soc2010_code == code, , drop = FALSE]
      if (nrow(matches) == 0) {
        out <- data[i, , drop = FALSE]
        out$isco08_code <- NA_character_
        out
      } else {
        do.call(rbind, lapply(seq_len(nrow(matches)), function(j) {
          out <- data[i, , drop = FALSE]
          out$isco08_code <- matches$isco08_code[j]
          out
        }))
      }
    }))
  }

  # If augment is FALSE, return only original columns plus mapping columns
  if (!augment) {
    # ensure mapped columns are last
    orig_cols <- names(data)
    res <- res[, c(orig_cols, setdiff(names(res), orig_cols))]
  }
  rownames(res) <- NULL
  res
}

#' Load ISCO-88 <-> ISCO-08 crosswalk
#'
#' Returns the mapping between ISCO-88 and ISCO-08 occupation codes from ILO/ILOSTAT.
#' This crosswalk is essential for integrating data coded in ISCO-88 (like Mahutga et al. 2018)
#' with ISCO-08 based datasets.
#' 
#' @section Many-to-Many Relationships:
#' When bridging ISCO-88 2-digit data (28 major groups) to ISCO-08 4-digit codes (427 occupations),
#' you will encounter **many-to-many relationships**. Multiple ISCO-08 occupations map to the same
#' ISCO-88 2-digit group, meaning they will share the same values from the ISCO-88 source.
#' 
#' For example, ISCO-88 group "11" (Legislators and senior officials) maps to multiple ISCO-08 codes:
#' \itemize{
#'   \item 1111 - Legislators
#'   \item 1112 - Senior government officials  
#'   \item 1113 - Traditional chiefs and heads of villages
#'   \item 1114 - Senior officials of special-interest organizations
#' }
#' All these ISCO-08 occupations will receive the same RTI/Offshorability value from Mahutga's "11" group.
#'
#' @section Usage Example:
#' \preformatted{
#' library(dplyr)
#' 
#' # Load datasets
#' mahutga <- load_mahutga()           # ISCO-88 2-digit (28 groups)
#' cw <- crosswalk_isco_88_08()   # Crosswalk
#' isco08 <- load_isco()               # ISCO-08 4-digit (427 occupations)
#' 
#' # Step 1: Extract 2-digit ISCO-88 codes from crosswalk
#' cw <- cw \%>\% mutate(isco88_2digit = substr(isco88_code, 1, 2))
#' 
#' # Step 2: Join crosswalk with Mahutga data
#' merged_cw <- left_join(cw, mahutga, by = c("isco88_2digit" = "isco88_code"))
#' 
#' # Step 3: Join with ISCO-08 data (use distinct() to avoid duplicates)
#' final <- left_join(isco08,
#'                    merged_cw \%>\% select(isco08_code, rti_mahutga_2018, ofs_mahutga_2018) \%>\% distinct(),
#'                    by = "isco08_code")
#' }
#'
#' @param path optional path to CSV file; if NULL uses inst/extdata/source_isco_88_08_crosswalk.csv
#' @return tibble with columns: isco88_code, isco88_title, isco08_code, isco08_title
#' @seealso \code{\link{load_mahutga}} for ISCO-88 2-digit RTI and Offshorability data
#' @export
crosswalk_isco_88_08 <- function(path = NULL) {
  message("Data source: International Labour Organization (ILO) ILOSTAT\n",
          "Crosswalk between ISCO-88 and ISCO-08 occupation classification systems\n",
          "Check website: https://ilostat.ilo.org/methods/concepts-and-definitions/classification-occupation/")
  
  if (is.null(path)) {
    path <- system.file("extdata", "source_isco_88_08_crosswalk.csv", package = "occupationRiskPack")
    if (is.null(path) || path == "") {
      candidates <- c(
        file.path("inst", "extdata", "source_isco_88_08_crosswalk.csv"),
        file.path("..", "inst", "extdata", "source_isco_88_08_crosswalk.csv")
      )
      found <- NULL
      for (c in candidates) if (file.exists(c)) { found <- c; break }
      if (is.null(found)) stop("source_isco_88_08_crosswalk.csv not found in inst/extdata/")
      path <- found
    }
  }
  
  df <- readr::read_csv(
    path, 
    col_select = 1:4,
    col_names = c("isco88_title", "isco88_code", "isco08_code", "isco08_title"),
    skip = 1,  # Skip the header row since we're providing names
    show_col_types = FALSE
  )
  
  # Convert codes to character and clean
  df$isco88_code <- as.character(df$isco88_code)
  df$isco08_code <- as.character(df$isco08_code)
  
  # Remove rows with missing codes
  df <- df[!is.na(df$isco88_code) & !is.na(df$isco08_code), ]
  df <- df[df$isco88_code != "" & df$isco08_code != "", ]
  
  return(dplyr::as_tibble(df))
}
