test_that("load_computerization() loads data correctly", {
  comp <- load_computerization()
  
  expect_s3_class(comp, "tbl_df")
  expect_true(nrow(comp) > 0)
  expect_true("soc2010_code" %in% names(comp))
  expect_true("soc2010_title" %in% names(comp))
  expect_true("computerization_risk" %in% names(comp))
})

test_that("load_computerization() has correct column types", {
  comp <- load_computerization()
  
  expect_type(comp$soc2010_code, "character")
  expect_type(comp$soc2010_title, "character")
  expect_type(comp$computerization_risk, "double")
})

test_that("load_computerization() SOC codes are formatted correctly", {
  comp <- load_computerization()
  
  # SOC 2010 codes should have format XX-XXXX
  expect_true(all(grepl("^[0-9]{2}-[0-9]{4}$", comp$soc2010_code)))
})

test_that("load_computerization() risk values are in valid range", {
  comp <- load_computerization()
  
  # Computerization risk should be probability (0-1)
  expect_true(all(comp$computerization_risk >= 0))
  expect_true(all(comp$computerization_risk <= 1))
  expect_true(min(comp$computerization_risk) < 0.01)  # Some low-risk jobs
  expect_true(max(comp$computerization_risk) >= 0.99)  # Some high-risk jobs (at least 0.99)
})

test_that("load_computerization() has expected number of occupations", {
  comp <- load_computerization()
  
  # Frey & Osborne (2017) analyzed 702 occupations
  expect_equal(nrow(comp), 702)
})

test_that("load_computerization() contains known occupations", {
  comp <- load_computerization()
  
  # Check for some known SOC codes
  soc_codes <- comp$soc2010_code
  
  # Should have manager codes
  expect_true(any(grepl("^11-", soc_codes)))
  
  # Should have various occupation types
  expect_true(length(unique(substr(soc_codes, 1, 2))) > 10)
})

test_that("load_computerization() has no missing values", {
  comp <- load_computerization()
  
  expect_false(any(is.na(comp$soc2010_code)))
  expect_false(any(is.na(comp$soc2010_title)))
  expect_false(any(is.na(comp$computerization_risk)))
})

test_that("load_computerization() can merge with SOC crosswalk", {
  comp <- load_computerization()
  soc_cw <- crosswalk_soc_10_18()
  
  # Try to merge SOC 2010 computerization with SOC 2018 crosswalk
  merged <- dplyr::left_join(soc_cw, comp, by = "soc2010_code")
  
  expect_s3_class(merged, "tbl_df")
  expect_true("computerization_risk" %in% names(merged))
  
  # Should have some matches
  n_matches <- sum(!is.na(merged$computerization_risk))
  expect_true(n_matches > 0)
})

test_that("load_computerization() prints citation message", {
  expect_message(load_computerization(), "Frey, C\\. B\\., & Osborne, M\\. A\\.")
  expect_message(load_computerization(), "Technological Forecasting and Social Change")
  expect_message(load_computerization(), "702 SOC 2010 occupations")
})

test_that("load_computerization() can identify high-risk occupations", {
  comp <- load_computerization()
  
  # Find occupations with >90% computerization risk
  high_risk <- comp[comp$computerization_risk > 0.9, ]
  
  expect_true(nrow(high_risk) > 0)
  expect_true(nrow(high_risk) < nrow(comp))  # Not all are high risk
})
