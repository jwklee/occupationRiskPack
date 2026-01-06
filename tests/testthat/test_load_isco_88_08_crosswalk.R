library(testthat)

test_that("crosswalk_isco_88_08 loads data correctly", {
  
  cw <- crosswalk_isco_88_08()
  
  expect_s3_class(cw, "tbl_df")
  expect_true(nrow(cw) > 0)
  expect_true(all(c("isco88_code", "isco88_title", "isco08_code", "isco08_title") %in% names(cw)))
})

test_that("crosswalk_isco_88_08 has correct column types", {
  
  cw <- crosswalk_isco_88_08()
  
  expect_type(cw$isco88_code, "character")
  expect_type(cw$isco88_title, "character")
  expect_type(cw$isco08_code, "character")
  expect_type(cw$isco08_title, "character")
})

test_that("crosswalk_isco_88_08 has no missing values in code columns", {
  
  cw <- crosswalk_isco_88_08()
  
  expect_false(any(is.na(cw$isco88_code)))
  expect_false(any(is.na(cw$isco08_code)))
})

test_that("crosswalk_isco_88_08 can join with load_mahutga using 2-digit codes", {
  
  mahutga <- load_mahutga(aggregate = "by_isco")
  cw <- crosswalk_isco_88_08()
  
  # Extract 2-digit ISCO-88 codes from crosswalk
  cw$isco88_2digit <- substr(cw$isco88_code, 1, 2)
  
  # Join with Mahutga data
  merged <- dplyr::left_join(cw, mahutga, by = c("isco88_2digit" = "isco88_code"))
  
  expect_s3_class(merged, "tbl_df")
  expect_true("rti_score" %in% names(merged))
  expect_true("offs_score" %in% names(merged))
  
  # Should have some matches (not all NA)
  expect_true(sum(!is.na(merged$rti_score)) > 0)
  expect_true(sum(!is.na(merged$offs_score)) > 0)
})

test_that("crosswalk_isco_88_08 can chain to ISCO-08 data", {
  
  isco08 <- base_isco()
  cw <- crosswalk_isco_88_08()
  
  # Join crosswalk with ISCO-08 data
  merged <- dplyr::left_join(cw, isco08, by = "isco08_code", relationship = "many-to-many")
  
  expect_s3_class(merged, "tbl_df")
  expect_true("isco08_code" %in% names(merged))
  
  # Check if any RTI data is present (column may exist in merged result)
  if ("rti_mihaylov_2019" %in% names(merged)) {
    expect_true(sum(!is.na(merged$rti_mihaylov_2019)) > 0)
  }
})

test_that("crosswalk_isco_88_08 prints citation message", {
  
  expect_message(crosswalk_isco_88_08(), "International Labour Organization")
  expect_message(crosswalk_isco_88_08(), "ISCO-88 and ISCO-08")
  expect_message(crosswalk_isco_88_08(), "https://ilostat.ilo.org")
})

test_that("Mahutga data can be bridged to ISCO-08 via crosswalk", {
  
  # Load all three datasets
  mahutga <- load_mahutga(aggregate = "by_isco")
  cw <- crosswalk_isco_88_08()
  isco08 <- base_isco()
  
  # Step 1: Join crosswalk with Mahutga (ISCO-88 2-digit)
  cw$isco88_2digit <- substr(cw$isco88_code, 1, 2)
  step1 <- dplyr::left_join(cw, mahutga, by = c("isco88_2digit" = "isco88_code"))
  
  # Step 2: Join with ISCO-08 data (many-to-many because crosswalk is not 1:1)
  final <- dplyr::left_join(
    isco08,
    step1 %>% dplyr::select(isco08_code, rti_score, offs_score),
    by = "isco08_code",
    relationship = "many-to-many"
  )
  
  expect_s3_class(final, "tbl_df")
  expect_true(nrow(final) >= nrow(isco08))  # May be more rows due to many-to-many
  expect_true("rti_score" %in% names(final))
  expect_true("offs_score" %in% names(final))
  
  # Should have some matches
  expect_true(sum(!is.na(final$rti_score)) > 0)
})
