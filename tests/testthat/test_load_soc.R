test_that("load_soc loads SOC 2018 data correctly", {
  
  soc <- base_soc()
  
  expect_true(is.data.frame(soc))
  expect_true(nrow(soc) > 1400)  # Should have 1447 rows
  expect_true(all(c("soc2018_code", "n_digits", "soc_group", "soc2018_title") %in% names(soc)))
  
  # Check all levels present
  expect_true(all(c("Major", "Minor", "Broad", "Detailed") %in% soc$soc_group))
})

test_that("load_soc filters by level correctly", {
  
  soc_detailed <- base_soc(level = "Detailed")
  expect_true(all(soc_detailed$soc_group == "Detailed"))
  expect_true(nrow(soc_detailed) >= 860)  # Should have 867 detailed codes
  
  soc_major <- base_soc(level = "Major")
  expect_true(all(soc_major$soc_group == "Major"))
  expect_equal(nrow(soc_major), 23)  # 23 major groups
})

test_that("crosswalk_soc_10_18 loads SOC 2010-2018 mapping", {
  
  cw <- crosswalk_soc_10_18()
  
  expect_true(is.data.frame(cw))
  expect_true(nrow(cw) >= 900)  # 900 mappings
  expect_true(all(c("soc2010_code", "soc2010_title", "soc2018_code", "soc2018_title") %in% names(cw)))
  
  # Check codes are character
  expect_true(is.character(cw$soc2010_code))
  expect_true(is.character(cw$soc2018_code))
})

test_that("SOC crosswalk can chain from ISCO via SOC 2010", {
  
  # Load SOC 2010 to 2018 crosswalk
  soc_cw <- crosswalk_soc_10_18()
  
  # Simulate a sample ISCO to SOC 2010 result
  sample_soc2010 <- data.frame(
    isco = c("1111", "2111"),
    soc = c("11-1011", "17-2051"),
    stringsAsFactors = FALSE
  )
  
  # Should be able to join them
  merged <- dplyr::left_join(
    sample_soc2010,
    soc_cw,
    by = c("soc" = "soc2010_code")
  )
  
  # Should have SOC 2018 columns after join
  expect_true("soc2018_code" %in% names(merged))
  expect_true(sum(!is.na(merged$soc2018_code)) > 0)
})

test_that("load_soc prints citation message", {
  
  expect_message(base_soc(), "US Bureau of Labor Statistics")
  expect_message(base_soc(), "Standard Occupational Classification 2018")
  expect_message(base_soc(), "https://www.bls.gov/soc/2018/")
  expect_message(base_soc(), "1447 SOC 2018 occupation codes")
})
