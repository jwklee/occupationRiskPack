test_that("impute_by_soc returns tibble with expected columns", {
  # Create test data with some missing values
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-1021", "11-1031", "11-2011", "11-2021", 
                     "13-1011", "13-1021", "13-2011"),
    value = c(100, NA, 120, 200, NA, 300, NA, 400),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value")
  
  expect_s3_class(result, "tbl_df")
  expect_true("value_with_imputed" %in% names(result))
  expect_true("value_with_imputed_impute_level" %in% names(result))
})

test_that("impute_by_soc preserves original non-NA values", {
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-1021", "11-1031"),
    value = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value")
  
  # Original values should be unchanged
 expect_equal(result$value_with_imputed, c(100, 200, 300))
  # Impute level should be 0 for original values
  expect_true(all(result$value_with_imputed_impute_level == 0))
})

test_that("impute_by_soc fills from 5-digit (Broad Occupation) level", {
  # Two codes in same Broad Occupation: 11-1011 and 11-1012
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-1012"),
    value = c(100, NA),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value")
  
  # NA should be imputed from 5-digit mean
  imputed_row <- result[result$soc2018_code == "11-1012", ]
  expect_equal(imputed_row$value_with_imputed, 100)
  expect_equal(imputed_row$value_with_imputed_impute_level, 5L)
})

test_that("impute_by_soc fills from 3-digit (Minor Group) level", {
  # Codes in same Minor Group (11-1) but different Broad Occupations
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-1021"),  # Same minor group 11-1
    value = c(100, NA),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value")
  
  # If 5-digit doesn't work, should impute from 3-digit
  imputed_row <- result[result$soc2018_code == "11-1021", ]
  expect_false(is.na(imputed_row$value_with_imputed))
})

test_that("impute_by_soc fills from 2-digit (Major Group) level", {
  # Codes in same Major Group (11) but different Minor Groups
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-9011"),  # Same major group 11
    value = c(100, NA),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value")
  
  # Should impute from 2-digit level
  imputed_row <- result[result$soc2018_code == "11-9011", ]
  expect_false(is.na(imputed_row$value_with_imputed))
  expect_equal(imputed_row$value_with_imputed_impute_level, 2L)
})

test_that("impute_by_soc respects custom soc_key parameter", {
  test_data <- data.frame(
    my_soc_code = c("11-1011", "11-1021"),
    value = c(100, NA),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value", soc_key = "my_soc_code")
  
  expect_s3_class(result, "tbl_df")
  expect_true("value_with_imputed" %in% names(result))
})

test_that("impute_by_soc respects custom new_var parameter", {
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-1021"),
    value = c(100, NA),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value", new_var = "my_imputed_value")
  
  expect_true("my_imputed_value" %in% names(result))
  expect_true("my_imputed_value_impute_level" %in% names(result))
})

test_that("impute_by_soc does not add temporary columns to output", {
  test_data <- data.frame(
    soc2018_code = c("11-1011", "11-1021"),
    value = c(100, NA),
    stringsAsFactors = FALSE
  )
  
  result <- impute_by_soc(test_data, "value")
  
  # Should not have temporary hierarchy columns
  expect_false("soc_5dg" %in% names(result))
  expect_false("soc_3dg" %in% names(result))
  expect_false("soc_2dg" %in% names(result))
  expect_false("mean_5dg" %in% names(result))
  expect_false("mean_3dg" %in% names(result))
  expect_false("mean_2dg" %in% names(result))
})

test_that("impute_by_soc works with real employment projections data", {
  skip_if_not(file.exists(system.file("extdata", "source_employment_projections.csv", 
                                        package = "occupationRiskPack")) ||
              file.exists("inst/extdata/source_employment_projections.csv"))
  
  emp <- suppressMessages(load_employment_projections())
  
  # Impute the wage column (which has some NAs)
  result <- impute_by_soc(emp, "median_annual_wage_2024")
  
  expect_s3_class(result, "tbl_df")
  expect_true("median_annual_wage_2024_with_imputed" %in% names(result))
  
  # Should have fewer NAs after imputation
  original_na <- sum(is.na(emp$median_annual_wage_2024))
  imputed_na <- sum(is.na(result$median_annual_wage_2024_with_imputed))
  expect_lte(imputed_na, original_na)
})
