library(testthat)

test_that("impute_by_isco fills from 3dg then 2dg", {
  df <- tibble::tibble(
    isco08_code = c("1111","1112","1130","1200","1210","1220"),
    val = c(1.0, NA, 0.8, NA, 0.6, NA)
  )
  out <- impute_by_isco(df, "val")
  expect_true("val_with_imputed" %in% names(out))
  expect_true("val_with_imputed_impute_level" %in% names(out))
  expect_true(all(is.na(out$val_with_imputed_impute_level) | out$val_with_imputed_impute_level %in% c(0L,2L,3L)))
})
