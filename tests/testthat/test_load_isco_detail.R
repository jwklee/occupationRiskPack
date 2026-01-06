test_that("base_isco_detail loads all levels correctly", {
  
  detail <- base_isco_detail()
  
  expect_true(is.data.frame(detail))
  expect_true(nrow(detail) > 600)  # Should have 619 rows
  expect_true(all(c("isco08_code", "n_digits", "isco08_title", "definition") %in% names(detail)))
  
  # Check all levels present
  expect_true(all(1:4 %in% detail$n_digits))
})

test_that("base_isco_detail filters by digits correctly", {
  
  detail_4 <- base_isco_detail(digits = 4)
  expect_true(all(detail_4$n_digits == 4))
  expect_true(nrow(detail_4) >= 430)  # Should have 436 4-digit codes
  
  detail_1 <- base_isco_detail(digits = 1)
  expect_true(all(detail_1$n_digits == 1))
  expect_equal(nrow(detail_1), 10)  # 10 major groups
})

test_that("base_isco_detail can merge with load_isco", {
  
  isco_base <- base_isco()
  detail_4 <- base_isco_detail(digits = 4)
  
  merged <- dplyr::left_join(
    isco_base,
    dplyr::select(detail_4, isco08_code, definition),
    by = "isco08_code"
  )
  
  # Should have same row count as base

expect_equal(nrow(merged), nrow(isco_base))
  
  # Most should have definitions
  expect_true(sum(!is.na(merged$definition)) > 400)
})
