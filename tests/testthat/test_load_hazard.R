# tests/testthat/test_load_hazard.R
library(testthat)

test_that("load_hazard returns expected structure", {
  expect_message(hazard <- load_hazard())
  expect_s3_class(hazard, "tbl_df")
  
  # Check expected columns
  expect_true("onet_soc2018_code" %in% names(hazard))
  expect_true("onet_soc2018_title" %in% names(hazard))
  expect_true("hazard_context" %in% names(hazard))
  expect_true("job_zone" %in% names(hazard))
})

test_that("load_hazard has correct number of occupations", {
  expect_message(hazard <- load_hazard())
  expect_equal(nrow(hazard), 894)
})

test_that("load_hazard returns correct column types", {
  expect_message(hazard <- load_hazard())
  expect_type(hazard$onet_soc2018_code, "character")
  expect_type(hazard$onet_soc2018_title, "character")
  expect_type(hazard$hazard_context, "integer")
  expect_type(hazard$job_zone, "integer")
})

test_that("hazard_context values are in expected range 0-100", {
  expect_message(hazard <- load_hazard())
  expect_true(all(hazard$hazard_context >= 0 & hazard$hazard_context <= 100))
})

test_that("job_zone values are in expected range 1-5", {
  expect_message(hazard <- load_hazard())
  expect_true(all(hazard$job_zone >= 1 & hazard$job_zone <= 5))
})

test_that("O*NET-SOC codes are 8-digit format with decimal", {
  expect_message(hazard <- load_hazard())
  # O*NET codes should be like "47-2152.00" (10 characters)
  expect_true(all(nchar(hazard$onet_soc2018_code) == 10))
  expect_true(all(grepl("^\\d{2}-\\d{4}\\.\\d{2}$", hazard$onet_soc2018_code)))
})

test_that("load_hazard contains expected granular codes", {
  expect_message(hazard <- load_hazard())
  
  # Check that granular O*NET codes are present
  expect_true("47-2152.00" %in% hazard$onet_soc2018_code)  # Plumbers base code
  expect_true("47-2152.04" %in% hazard$onet_soc2018_code)  # Solar Thermal Installers
  expect_true("19-1029.01" %in% hazard$onet_soc2018_code)  # Bioinformatics Scientists
  expect_true("19-1029.02" %in% hazard$onet_soc2018_code)  # Molecular and Cellular Biologists
})

test_that("load_hazard citation message is printed", {
  expect_message(load_hazard(), "O\\*NET Hazardous Conditions")
  expect_message(load_hazard(), "894 occupations")
})

test_that("hazard scores match expected values for specific occupations", {
  expect_message(hazard <- load_hazard())
  
  # Plumbers should have hazard score 54
  plumbers <- hazard[hazard$onet_soc2018_code == "47-2152.00", ]
  expect_equal(nrow(plumbers), 1)
  expect_equal(plumbers$hazard_context, 54L)
  
  # Bioinformatics Scientists should have hazard score 11
  bioinf <- hazard[hazard$onet_soc2018_code == "19-1029.01", ]
  expect_equal(nrow(bioinf), 1)
  expect_equal(bioinf$hazard_context, 11L)
})

test_that("job_zone values match expected values for specific occupations", {
  expect_message(hazard <- load_hazard())
  
  # Plumbers should be job zone 3
  plumbers <- hazard[hazard$onet_soc2018_code == "47-2152.00", ]
  expect_equal(plumbers$job_zone, 3L)
  
  # Bioinformatics Scientists should be job zone 5
  bioinf <- hazard[hazard$onet_soc2018_code == "19-1029.01", ]
  expect_equal(bioinf$job_zone, 5L)
})

test_that("can extract 6-digit SOC codes from O*NET codes", {
  expect_message(hazard <- load_hazard())
  
  # Extract standard 6-digit SOC code (first 7 chars including hyphen)
  hazard$soc2018_code <- substr(hazard$onet_soc2018_code, 1, 7)
  
  # Check that we can aggregate by 6-digit code
  plumbers_soc <- hazard[hazard$soc2018_code == "47-2152", ]
  expect_equal(nrow(plumbers_soc), 2)  # 47-2152.00 and 47-2152.04
  
  biologists_soc <- hazard[hazard$soc2018_code == "19-1029", ]
  expect_equal(nrow(biologists_soc), 4)  # 4 sub-codes
})

test_that("can aggregate hazard scores to 6-digit SOC level", {
  expect_message(hazard <- load_hazard())
  
  hazard$soc2018_code <- substr(hazard$onet_soc2018_code, 1, 7)
  
  # Aggregate biologists (19-1029)
  bio_agg <- aggregate(hazard_context ~ soc2018_code, 
                       data = hazard[hazard$soc2018_code == "19-1029", ],
                       FUN = mean)
  
  # Mean of 11, 51, 50, 22 = 33.5
  expect_equal(bio_agg$hazard_context, mean(c(11, 51, 50, 22)))
})

test_that("hazard scores span full range", {
  expect_message(hazard <- load_hazard())
  
  # Check that extreme values exist
  expect_true(any(hazard$hazard_context == 0))    # Never exposed
  expect_true(any(hazard$hazard_context >= 95))   # Very frequently exposed
})

test_that("all job zones 1-5 are represented", {
  expect_message(hazard <- load_hazard())
  
  expect_true(1 %in% hazard$job_zone)
  expect_true(2 %in% hazard$job_zone)
  expect_true(3 %in% hazard$job_zone)
  expect_true(4 %in% hazard$job_zone)
  expect_true(5 %in% hazard$job_zone)
})

test_that("no missing values in key columns", {
  expect_message(hazard <- load_hazard())
  
  expect_false(any(is.na(hazard$onet_soc2018_code)))
  expect_false(any(is.na(hazard$hazard_context)))
  expect_false(any(is.na(hazard$job_zone)))
})
