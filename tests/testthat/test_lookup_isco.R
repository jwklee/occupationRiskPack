library(testthat)

test_that("lookup_isco works: codes to titles", {
  # Create toy data
  df <- data.frame(
    isco_08_4dg = c('1111','1112','1120','2000','2110','3323'),
    occupation_title = c('Chief Executives','General Managers','Managers','Architects','Engineers','Electronics Technicians'),
    stringsAsFactors = FALSE
  )

  # Lookup titles for given codes
  result <- lookup_isco(df, lookup_values = c("1111", "2110"), 
                        lookup_col = "isco_08_4dg", return_col = "occupation_title")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c('isco_08_4dg','occupation_title') %in% names(result)))
  expect_true(all(result$isco_08_4dg %in% c("1111", "2110")))
})

test_that("lookup_isco works: titles to codes (case-insensitive)", {
  df <- data.frame(
    isco_08_4dg = c('1111','1112','1120','2000','2110','3323'),
    occupation_title = c('Chief Executives','General Managers','Managers','Architects','Engineers','Electronics Technicians'),
    stringsAsFactors = FALSE
  )

  # Lookup codes for given titles (case-insensitive)
  result <- lookup_isco(df, lookup_values = c("engineers", "ARCHITECTS"), 
                        lookup_col = "occupation_title", return_col = "isco_08_4dg")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c('occupation_title','isco_08_4dg') %in% names(result)))
  expect_true(all(result$isco_08_4dg %in% c("2110", "2000")))
})

test_that("lookup_isco handles no matches gracefully", {
  df <- data.frame(
    isco_08_4dg = c('1111','1112'),
    occupation_title = c('Chief Executives','General Managers'),
    stringsAsFactors = FALSE
  )

  # Code that doesn't exist
  result <- lookup_isco(df, lookup_values = c("9999"), 
                        lookup_col = "isco_08_4dg", return_col = "occupation_title")
  expect_equal(nrow(result), 0)
  expect_true(is.data.frame(result))
})

test_that("lookup_isco requires lookup_col parameter", {
  df <- data.frame(
    isco_08_4dg = c('1111'),
    occupation_title = c('Chief Executives'),
    stringsAsFactors = FALSE
  )

  # Missing lookup_col
  expect_error(lookup_isco(df, lookup_values = "1111", return_col = "occupation_title"), 
               "`lookup_col` must be specified")
})

test_that("lookup_isco requires return_col parameter", {
  df <- data.frame(
    isco_08_4dg = c('1111'),
    occupation_title = c('Chief Executives'),
    stringsAsFactors = FALSE
  )

  # Missing return_col
  expect_error(lookup_isco(df, lookup_values = "1111", lookup_col = "isco_08_4dg"), 
               "`return_col` must be specified")
})

test_that("lookup_isco respects case_insensitive flag", {
  df <- data.frame(
    isco_08_4dg = c('1111','1112'),
    occupation_title = c('Chief Executives','General Managers'),
    stringsAsFactors = FALSE
  )

  # Case-sensitive lookup (should fail to match lowercase)
  result <- lookup_isco(df, lookup_values = c("chief executives"), 
                        lookup_col = "occupation_title", return_col = "isco_08_4dg",
                        case_insensitive = FALSE)
  expect_equal(nrow(result), 0)

  # Case-insensitive lookup (should match)
  result <- lookup_isco(df, lookup_values = c("chief executives"), 
                        lookup_col = "occupation_title", return_col = "isco_08_4dg",
                        case_insensitive = TRUE)
  expect_equal(nrow(result), 1)
})
