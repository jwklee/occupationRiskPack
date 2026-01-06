test_that("load_employment_projections returns tibble with expected structure", {
  df <- suppressMessages(load_employment_projections())
  
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 832)
  expect_equal(ncol(df), 14)
})

test_that("load_employment_projections has expected column names", {
  df <- suppressMessages(load_employment_projections())
  
  expected_cols <- c(
    "soc2018_title", "soc2018_code",
    "employment_2024_thousands", "employment_2034_thousands",
    "employment_change_24to34_thousands", "employment_change_24to34_percent",
    "employment_change_24to34_thousands_annual_average",
    "median_annual_wage_2024",
    "typical_entry_level_edu", "education_code",
    "work_experience_in_a_related_occupation", "work_exp_code",
    "typical_ojt", "ojt_code"
  )
  
  expect_equal(names(df), expected_cols)
})

test_that("load_employment_projections has correct column types", {
  df <- suppressMessages(load_employment_projections())
  
  # SOC code should be character
  expect_type(df$soc2018_code, "character")
  expect_type(df$soc2018_title, "character")
  
  # Numeric columns
  expect_type(df$employment_2024_thousands, "double")
  expect_type(df$employment_2034_thousands, "double")
  expect_type(df$employment_change_24to34_thousands, "double")
  expect_type(df$employment_change_24to34_percent, "double")
  expect_type(df$median_annual_wage_2024, "double")
  
  # Code columns should be integer
  expect_type(df$education_code, "integer")
  expect_type(df$work_exp_code, "integer")
  expect_type(df$ojt_code, "integer")
})

test_that("load_employment_projections has valid SOC 2018 codes", {
  df <- suppressMessages(load_employment_projections())
  
  # All codes should match SOC 2018 format: XX-XXXX
  valid_format <- grepl("^[0-9]{2}-[0-9]{4}$", df$soc2018_code)
  expect_true(all(valid_format))
  
  # No corrupted Excel date values
  expect_false(any(grepl("Nov|Dec|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct", 
                         df$soc2018_code, ignore.case = TRUE)))
})

test_that("load_employment_projections wage values are reasonable", {
  df <- suppressMessages(load_employment_projections())
  
  # Wages should be positive where not NA
  valid_wages <- df$median_annual_wage_2024[!is.na(df$median_annual_wage_2024)]
  expect_true(all(valid_wages > 0))
  expect_true(all(valid_wages <= 239200))  # Top-coded value
  
  # Only 6 NA values
  expect_equal(sum(is.na(df$median_annual_wage_2024)), 6)
})

test_that("load_employment_projections has clean occupation titles", {
  df <- suppressMessages(load_employment_projections())
  
  # Titles should not have asterisk suffixes (from alternate titles)
  has_asterisk_suffix <- grepl("[[:space:]]{2,}[*]", df$soc2018_title)
  expect_true(all(!has_asterisk_suffix))
})

test_that("load_employment_projections displays citation message", {
  expect_message(load_employment_projections(), "Bureau of Labor Statistics")
  expect_message(load_employment_projections(), "2024-34 Employment Projections")
  expect_message(load_employment_projections(), "top-coded")
})
