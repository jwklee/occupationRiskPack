library(testthat)

test_that("crosswalk expands many-to-many mappings and maps both directions", {
  # Create a small toy crosswalk (we won't read excel here; create in-memory)
  cw <- data.frame(isco08_code = c('3323','3323','2000'), soc2010_code = c('13-1021','13-1022','11-1011'), stringsAsFactors = FALSE)

  # Write to a temporary excel file so loader can read it (requires readxl installed)
  tmp <- tempfile(fileext = '.xlsx')
  if (requireNamespace('writexl', quietly = TRUE)) {
    writexl::write_xlsx(cw, tmp)
  } else {
    skip('writexl not available to write temp crosswalk file')
  }

  # Load via loader
  cw_loaded <- crosswalk_isco_08_soc_10(path = tmp)
  expect_true(is.data.frame(cw_loaded))
  expect_true(all(c('isco08_code','soc2010_code') %in% names(cw_loaded)))

  # Prepare a data.frame with ISCO codes to map to SOC (many-to-many expected)
  df_isco <- data.frame(id = 1:2, isco_08_4dg = c('3323','2000'), stringsAsFactors = FALSE)
  mapped <- crosswalk_isco_soc(data = df_isco, from = 'isco', code_col = 'isco_08_4dg', path = tmp, augment = FALSE)
  # Expect rows: 3323 -> two SOCs => 2 rows for id 1; 2000 -> 1 mapping => 1 row for id 2
  expect_equal(nrow(mapped), 3)
  expect_true('soc2010_code' %in% names(mapped))
  expect_equal(sum(mapped$id == 1), 2)

  # Now map back SOC -> ISCO
  df_soc <- data.frame(id = 1:3, soc2010_code = c('13-1021','13-1022','11-1011'), stringsAsFactors = FALSE)
  mapped2 <- crosswalk_isco_soc(data = df_soc, from = 'soc', code_col = 'soc2010_code', path = tmp, augment = FALSE)
  expect_equal(nrow(mapped2), 3)
  expect_true(all(mapped2$soc2010_code_type == 'isco'))
})
