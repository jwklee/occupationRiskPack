# tests/testthat/test_load_mahutga.R
library(testthat)

test_that("load_mahutga returns expected structure with aggregate='none'", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  expect_s3_class(mahutga, "tbl_df")
  
  # Check expected columns
  expect_true("country" %in% names(mahutga))
  expect_true("year" %in% names(mahutga))
  expect_true("isco88_code" %in% names(mahutga))
  expect_true("rti_score" %in% names(mahutga))
  expect_true("offs_score" %in% names(mahutga))
  expect_equal(ncol(mahutga), 5)
})

test_that("load_mahutga has expected data dimensions", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  
  # Should have ~5331 rows
  expect_gt(nrow(mahutga), 5000)
  expect_lt(nrow(mahutga), 6000)
  
  # Check counts
  expect_equal(length(unique(mahutga$isco88_code)), 27)
  expect_gt(length(unique(mahutga$country)), 30)  # ~40+ countries
  expect_gt(length(unique(mahutga$year)), 20)     # many years
})

test_that("load_mahutga returns correct column types", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  expect_type(mahutga$country, "character")
  expect_type(mahutga$year, "integer")
  expect_type(mahutga$isco88_code, "character")
  expect_type(mahutga$rti_score, "double")
  expect_type(mahutga$offs_score, "double")
})

test_that("country codes are 2-letter uppercase", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  expect_true(all(nchar(mahutga$country) == 2))
  expect_true(all(mahutga$country == toupper(mahutga$country)))
})

test_that("years are 4-digit", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  expect_true(all(mahutga$year >= 1970))
  expect_true(all(mahutga$year <= 2020))
})

test_that("ISCO-88 codes are 2-digit", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  expect_true(all(nchar(mahutga$isco88_code) == 2))
  expect_true(all(grepl("^[0-9]{2}$", mahutga$isco88_code)))
})

test_that("load_mahutga citation message is printed", {
  expect_message(load_mahutga(), "Mahutga")
  expect_message(load_mahutga(), "2018")
  expect_message(load_mahutga(), "ISCO-88 2-digit")
})

# Test year filtering
test_that("years parameter filters correctly", {
  expect_message(mahutga_2010 <- load_mahutga(years = 2010))
  expect_true(all(mahutga_2010$year == 2010))
  
  expect_message(mahutga_multi <- load_mahutga(years = c(2007, 2010)))
  expect_true(all(mahutga_multi$year %in% c(2007, 2010)))
})

# Test country filtering
test_that("countries parameter filters correctly", {
  expect_message(mahutga_de <- load_mahutga(countries = "DE"))
  expect_true(all(mahutga_de$country == "DE"))
  
  expect_message(mahutga_multi <- load_mahutga(countries = c("DE", "FR")))
  expect_true(all(mahutga_multi$country %in% c("DE", "FR")))
})

test_that("countries parameter is case-insensitive", {
  expect_message(mahutga_lower <- load_mahutga(countries = "de"))
  expect_true(all(mahutga_lower$country == "DE"))
})

test_that("invalid country gives warning", {
  expect_warning(
    expect_message(load_mahutga(countries = "INVALID")),
    "Countries not found"
  )
})

# Test aggregation by_isco
test_that("aggregate='by_isco' collapses to ISCO level", {
  expect_message(mahutga_agg <- load_mahutga(aggregate = "by_isco"))
  
  # Should have one row per ISCO code
  expect_equal(nrow(mahutga_agg), 27)
  expect_equal(length(unique(mahutga_agg$isco88_code)), 27)
  
  # Should have only isco88_code, rti_score, offs_score columns
  expect_true("isco88_code" %in% names(mahutga_agg))
  expect_true("rti_score" %in% names(mahutga_agg))
  expect_true("offs_score" %in% names(mahutga_agg))
  expect_equal(ncol(mahutga_agg), 3)
  
  # Should NOT have country/year columns
  expect_false("country" %in% names(mahutga_agg))
  expect_false("year" %in% names(mahutga_agg))
})

# Test combined filtering and aggregation
test_that("can filter and aggregate together", {
  expect_message(mahutga <- load_mahutga(
    aggregate = "by_isco",
    years = 2010,
    countries = c("DE", "FR")
  ))
  
  # Should have aggregated to ISCO level with only 3 columns
  expect_true("isco88_code" %in% names(mahutga))
  expect_true("rti_score" %in% names(mahutga))
  expect_true("offs_score" %in% names(mahutga))
  expect_equal(ncol(mahutga), 3)
})

# Test specific values
test_that("RTI and Offshorability scores have expected range", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  
  # RTI can be negative or positive
  expect_true(any(mahutga$rti_score < 0))
  expect_true(any(mahutga$rti_score > 0))
  
  # Offs can be negative or positive
  expect_true(any(mahutga$offs_score < 0))
  expect_true(any(mahutga$offs_score > 0))
})

test_that("country-year variation exists in data", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  
  # Same ISCO code should have different values across country-years
  isco_11 <- mahutga[mahutga$isco88_code == "11", ]
  expect_gt(nrow(isco_11), 1)
  
  # Should have multiple countries/years
  expect_gt(length(unique(isco_11$country)), 1)
})

test_that("no missing values in key columns", {
  expect_message(mahutga <- load_mahutga(aggregate = "none"))
  
  expect_false(any(is.na(mahutga$country)))
  expect_false(any(is.na(mahutga$year)))
  expect_false(any(is.na(mahutga$isco88_code)))
  expect_false(any(is.na(mahutga$rti_score)))
  expect_false(any(is.na(mahutga$offs_score)))
})

test_that("aggregated data has no missing values", {
  expect_message(mahutga_agg <- load_mahutga(aggregate = "by_isco"))
  
  expect_false(any(is.na(mahutga_agg$isco88_code)))
  expect_false(any(is.na(mahutga_agg$rti_score)))
  expect_false(any(is.na(mahutga_agg$offs_score)))
})

test_that("contains expected ISCO-88 2-digit codes", {
  expect_message(mahutga <- load_mahutga(aggregate = "by_isco"))
  
  codes <- mahutga$isco88_code
  
  # Should have managers (11-13)
  expect_true(any(codes %in% c("11", "12", "13")))
  
  # Should have professionals (21-24)
  expect_true(any(codes %in% c("21", "22", "23", "24")))
  
  # Should have elementary occupations (91-93)
  expect_true(any(codes %in% c("91", "92", "93")))
})
