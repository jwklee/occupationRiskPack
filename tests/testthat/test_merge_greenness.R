library(testthat)


test_that("load_brownness reads extracted Table A1 and has expected rows", {
  gre <- load_brownness()
  expect_true(is.data.frame(gre))
  expect_true(nrow(gre) >= 150)
  expect_true(all(c("isco08_code", "greenness", "brownness") %in% names(gre)))
})

test_that("manual join attaches greenness and keeps row count", {
  isco <- base_isco()
  gre <- load_brownness()

  if (!"isco08_code" %in% names(isco)) skip("isco file missing isco08_code column")

  merged <- dplyr::left_join(isco, gre, by = "isco08_code")

  expect_equal(nrow(merged), nrow(isco))
  expect_true("greenness" %in% names(merged))
  expect_true("brownness" %in% names(merged))
  n_matched <- sum(!is.na(merged$greenness))
  expect_true(n_matched >= 140)
})
