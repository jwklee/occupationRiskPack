test_that("load_offshorability() loads data correctly", {
  offsh <- load_offshorability()
  
  expect_s3_class(offsh, "tbl_df")
  expect_true(nrow(offsh) > 0)
  expect_true("soc2010_code" %in% names(offsh))
  expect_true("soc2010_title" %in% names(offsh))
  expect_true("offshorability_index" %in% names(offsh))
})

test_that("load_offshorability() has correct column types", {
  offsh <- load_offshorability()
  
  expect_type(offsh$soc2010_code, "character")
  expect_type(offsh$soc2010_title, "character")
  expect_type(offsh$offshorability_index, "double")
})

test_that("load_offshorability() SOC codes are formatted with hyphen", {
  offsh <- load_offshorability()
  
  # All SOC codes should have format XX-XXXX
  expect_true(all(grepl("^[0-9]{2}-[0-9]{4}$", offsh$soc2010_code)))
})

test_that("load_offshorability() offshorability values are in reasonable range", {
  offsh <- load_offshorability()
  
  # Based on Blinder (2007), offshorability index ranges from ~25-100
  expect_true(all(offsh$offshorability_index >= 25))
  expect_true(all(offsh$offshorability_index <= 100))
  expect_true(min(offsh$offshorability_index) == 25)
  expect_true(max(offsh$offshorability_index) == 100)
})

test_that("load_offshorability() has expected number of occupations", {
  offsh <- load_offshorability()
  
  # Blinder (2007) ranks 291 occupations, but we only extracted those with
  # offshorability index values (some entries in PDF were incomplete)
  expect_true(nrow(offsh) >= 200)  # Should have most occupations
  expect_true(nrow(offsh) <= 300)  # But not more than original 291
})

test_that("load_offshorability() contains known high-offshorability occupations", {
  offsh <- load_offshorability()
  
  # Computer Programmers should have offshorability = 100
  comp_prog <- offsh[offsh$soc2010_code == "15-1021", ]
  expect_equal(nrow(comp_prog), 1)
  expect_equal(comp_prog$offshorability_index, 100)
  
  # Data Entry Keyers should have offshorability = 100
  data_entry <- offsh[offsh$soc2010_code == "43-9021", ]
  expect_equal(nrow(data_entry), 1)
  expect_equal(data_entry$offshorability_index, 100)
})

test_that("load_offshorability() has no missing values", {
  offsh <- load_offshorability()
  
  expect_false(any(is.na(offsh$soc2010_code)))
  expect_false(any(is.na(offsh$soc2010_title)))
  expect_false(any(is.na(offsh$offshorability_index)))
})

test_that("load_offshorability() can merge with SOC crosswalk", {
  offsh <- load_offshorability()
  soc_cw <- crosswalk_soc_10_18()
  
  # Merge SOC 2010 offshorability with SOC 2010->2018 crosswalk
  merged <- dplyr::left_join(soc_cw, offsh, by = "soc2010_code", relationship = "many-to-many")
  
  expect_s3_class(merged, "tbl_df")
  expect_true("offshorability_index" %in% names(merged))
  
  # Should have some matches
  n_matches <- sum(!is.na(merged$offshorability_index))
  expect_true(n_matches > 0)
})

test_that("load_offshorability() prints citation message", {
  expect_message(load_offshorability(), "O\\*NET data 2006 version 10\\.0")
  expect_message(load_offshorability(), "Blinder, A\\. S\\.")
  expect_message(load_offshorability(), "CEPS Working Paper No\\. 142")
})
