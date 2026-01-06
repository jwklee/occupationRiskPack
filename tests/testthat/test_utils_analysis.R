library(testthat)

test_that("get_top_bottom_occupations returns expected structure and rankings", {
  # Create toy data: 6 occupations with varying RTI scores
  df <- data.frame(
    isco08_code = c('1111','1112','1120','2000','2110','3323'),
    occupation_title = c('Chief Executives','General Managers','Managers','Architects','Engineers','Electronics Technicians'),
    rti_mihaylov_2019 = c(0.15, 0.18, 0.20, 0.45, 0.50, 0.62),
    stringsAsFactors = FALSE
  )

  # Test: overall top/bottom (n=2 means 2 top + 2 bottom = 4 rows)
  result <- get_top_bottom_occupations(df, var = "rti_mihaylov_2019", n = 2)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)  # 2 top + 2 bottom
  expect_true(all(c('isco08_code','rti_mihaylov_2019','percentile','rank_type') %in% names(result)))
  # Should NOT have duplicate isco08_code columns
  expect_equal(sum(names(result) == 'isco08_code'), 1)

  # Verify top and bottom
  top_ranks <- result[result$rank_type == 'top', ]
  bottom_ranks <- result[result$rank_type == 'bottom', ]
  expect_equal(nrow(top_ranks), 2)
  expect_equal(nrow(bottom_ranks), 2)
  # Top should be highest RTI values (0.62, 0.50)
  expect_true(all(top_ranks$rti_mihaylov_2019 >= 0.50))
  # Bottom should be lowest (0.15, 0.18)
  expect_true(all(bottom_ranks$rti_mihaylov_2019 <= 0.20))

  # Test: grouped by ISCO 1-digit
  result_grouped <- get_top_bottom_occupations(df, var = "rti_mihaylov_2019", n = 1, by_group = 1)
  expect_true('isco_prefix' %in% names(result_grouped))
  # Should have rows grouped by major occupation groups
  expect_true(length(unique(result_grouped$isco_prefix)) > 1)
})

test_that("get_top_bottom_occupations handles missing values correctly", {
  df <- data.frame(
    isco08_code = c('1111','1112','1120','2000'),
    occupation_title = c('Chief','Manager','Mgr2','Architect'),
    rti_mihaylov_2019 = c(0.15, NA, 0.20, 0.45),
    stringsAsFactors = FALSE
  )

  # Should skip NA and still work
  result <- get_top_bottom_occupations(df, var = "rti_mihaylov_2019", n = 1)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)  # 1 top + 1 bottom (3 non-NA values)
  expect_true(all(!is.na(result$rti_mihaylov_2019)))
})
