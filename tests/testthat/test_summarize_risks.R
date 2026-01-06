library(testthat)

test_that("summarize_risks minimal implementation returns expected structure", {
  df <- data.frame(
    isco08_code = c('1111','1112','1120','1210','2000','2001'),
    rti = c(1.2, NA, 2.5, 0.9, 3.0, NA),
    greenness = c(0.1, 0.2, NA, 0.4, 0.5, 0.6),
    stringsAsFactors = FALSE
  )

  out <- summarize_risks(data = df, vars = c('rti', 'greenness'), isco_key = 'isco08_code', digits = c(1,2))

  # now a data.frame with rows for overall and by-digit groups
  expect_true(is.data.frame(out))
  expect_true(all(c('isco_digit','isco_prefix','variable','n','n_non_missing','mean','sd','median','missing_pct') %in% names(out)))

  # should contain overall rows (isco_digit == 'overall') and digit rows '1' and '2'
  expect_true('overall' %in% unique(out$isco_digit))
  expect_true('1' %in% unique(out$isco_digit))
  expect_true('2' %in% unique(out$isco_digit))

  # overall should have one row per variable
  overall_rows <- out[out$isco_digit == 'overall', ]
  expect_equal(nrow(overall_rows), 2)
})
