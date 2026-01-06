# tests/testthat/test_load_skill_specificity.R
library(testthat)

test_that("load_skill_specificity returns expected structure with aggregate='none'", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  expect_s3_class(ss, "tbl_df")
  
  # Check expected columns
  expect_true("country" %in% names(ss))
  expect_true("ess_wave" %in% names(ss))
  expect_true("isco88_code" %in% names(ss))
  expect_true("skill_specificity" %in% names(ss))
})

test_that("load_skill_specificity has expected data dimensions", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  
  # Should have ~29647 rows (country x wave x ISCO combinations)
  expect_gt(nrow(ss), 29000)
  expect_lt(nrow(ss), 30000)
  
  # Check counts
  expect_equal(length(unique(ss$country)), 32)
  expect_equal(length(unique(ss$ess_wave)), 5)
  expect_equal(length(unique(ss$isco88_code)), 492)
})

test_that("load_skill_specificity returns correct column types", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  expect_type(ss$country, "character")
  expect_type(ss$ess_wave, "integer")
  expect_type(ss$isco88_code, "character")
  expect_type(ss$skill_specificity, "double")
})

test_that("ESS waves are 1-5", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  expect_true(all(ss$ess_wave %in% 1:5))
})

test_that("skill_specificity values are positive", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  expect_true(all(ss$skill_specificity > 0, na.rm = TRUE))
})

test_that("load_skill_specificity citation message is printed", {
  expect_message(load_skill_specificity(), "Pardos-Prado")
  expect_message(load_skill_specificity(), "Iversen")
  expect_message(load_skill_specificity(), "492 ISCO-88")
})

# Test wave filtering
test_that("wave parameter filters correctly", {
  expect_message(ss5 <- load_skill_specificity(wave = 5))
  expect_true(all(ss5$ess_wave == 5))
  
  expect_message(ss13 <- load_skill_specificity(wave = c(1, 3)))
  expect_true(all(ss13$ess_wave %in% c(1, 3)))
})

test_that("invalid wave parameter throws error", {
  expect_error(load_skill_specificity(wave = 6), "wave must be integer")
  expect_error(load_skill_specificity(wave = 0), "wave must be integer")
})

# Test country filtering
test_that("countries parameter filters correctly", {
  expect_message(ss_de <- load_skill_specificity(countries = "DE"))
  expect_true(all(ss_de$country == "DE"))
  
  expect_message(ss_multi <- load_skill_specificity(countries = c("DE", "FR")))
  expect_true(all(ss_multi$country %in% c("DE", "FR")))
})

test_that("countries parameter is case-insensitive", {
  expect_message(ss_lower <- load_skill_specificity(countries = "de"))
  expect_true(all(ss_lower$country == "DE"))
})

test_that("invalid country gives warning", {
  expect_warning(
    expect_message(load_skill_specificity(countries = "INVALID")),
    "Countries not found"
  )
})

# Test aggregation by_isco
test_that("aggregate='by_isco' collapses to ISCO level", {
  expect_message(ss_agg <- load_skill_specificity(aggregate = "by_isco"))
  
  # Should have one row per ISCO code
  expect_equal(nrow(ss_agg), 492)
  expect_equal(length(unique(ss_agg$isco88_code)), 492)
  
  # Should have only isco88_code and skill_specificity columns
  expect_true("isco88_code" %in% names(ss_agg))
  expect_true("skill_specificity" %in% names(ss_agg))
  expect_equal(ncol(ss_agg), 2)
  
  # Should NOT have country/wave columns
  expect_false("country" %in% names(ss_agg))
  expect_false("ess_wave" %in% names(ss_agg))
})

# Test aggregation by_isco_wave
test_that("aggregate='by_isco_wave' collapses to ISCO-wave level", {
  expect_message(ss_agg <- load_skill_specificity(aggregate = "by_isco_wave"))
  
  # Should have only isco88_code, ess_wave, and skill_specificity columns
  expect_true("isco88_code" %in% names(ss_agg))
  expect_true("ess_wave" %in% names(ss_agg))
  expect_true("skill_specificity" %in% names(ss_agg))
  expect_equal(ncol(ss_agg), 3)
  
  # Should NOT have country column
  expect_false("country" %in% names(ss_agg))
})

test_that("aggregate='by_isco_wave' with wave=5 gives recent data", {
  expect_message(ss_wave5 <- load_skill_specificity(aggregate = "by_isco_wave", wave = 5))
  
  # All rows should be wave 5
  expect_true(all(ss_wave5$ess_wave == 5))
  
  # Should have one row per ISCO code for wave 5
  expect_equal(nrow(ss_wave5), length(unique(ss_wave5$isco88_code)))
})

# Test combined filtering and aggregation
test_that("can filter and aggregate together", {
  expect_message(ss <- load_skill_specificity(
    aggregate = "by_isco",
    wave = 5,
    countries = c("DE", "FR", "GB")
  ))
  
  # Should have aggregated to ISCO level with only 2 columns
  expect_true("isco88_code" %in% names(ss))
  expect_true("skill_specificity" %in% names(ss))
  expect_equal(ncol(ss), 2)
})

# Test specific values
test_that("specific ISCO codes have expected variation pattern", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  
  # ISCO 1110 should be present
  ss_1110 <- ss[ss$isco88_code == "1110", ]
  expect_gt(nrow(ss_1110), 0)
  
  # Should have variation across countries
  expect_gt(length(unique(ss_1110$country)), 1)
})

test_that("Ireland and Norway 1110 values differ as documented", {
  expect_message(ss <- load_skill_specificity(aggregate = "none"))
  
  # Wave 5 (2010) values
  ie_w5 <- ss[ss$isco88_code == "1110" & ss$country == "IE" & ss$ess_wave == 5, ]
  no_w5 <- ss[ss$isco88_code == "1110" & ss$country == "NO" & ss$ess_wave == 5, ]
  
  if (nrow(ie_w5) > 0 && nrow(no_w5) > 0) {
    # Values should differ (demonstrating country variation)
    expect_false(ie_w5$skill_specificity == no_w5$skill_specificity)
  }
})

test_that("no missing ISCO codes in aggregated output", {
  expect_message(ss_agg <- load_skill_specificity(aggregate = "by_isco"))
  expect_false(any(is.na(ss_agg$isco88_code)))
  expect_false(any(is.na(ss_agg$skill_specificity)))
})

test_that("ISCO-88 codes are 4-digit", {
  expect_message(ss <- load_skill_specificity(aggregate = "by_isco"))
  # Most ISCO-88 codes should be 4 digits
  expect_true(mean(nchar(ss$isco88_code) == 4) > 0.9)
})
