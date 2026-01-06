test_that("minimal API loaders work and can be joined manually", {
  isco <- base_isco()
  rti <- load_rti()
  gre <- load_brownness()

  expect_true(is.data.frame(isco))
  expect_true(is.data.frame(rti))
  expect_true(is.data.frame(gre))

  expect_true("isco08_code" %in% names(isco))
  expect_true("isco08_code" %in% names(rti))
  expect_true("isco08_code" %in% names(gre))

  # Manual merge using dplyr - keep only needed columns from rti and gre
  # to avoid isco08_title conflicts
  merged <- isco %>%
    dplyr::left_join(
      rti %>% dplyr::select(isco08_code, dplyr::starts_with("rti_"), dplyr::starts_with("nr"), dplyr::starts_with("r")),
      by = "isco08_code"
    ) %>%
    dplyr::left_join(
      gre %>% dplyr::select(isco08_code, greenness, brownness, brown_dummy),
      by = "isco08_code"
    )

  expect_equal(nrow(isco), nrow(merged))

  # Should not have .x/.y suffixes if we selected properly
  suffixed <- grep("\\.x$|\\.y$", names(merged), value = TRUE)
  expect_length(suffixed, 0)

  expect_true("isco08_code" %in% names(merged))
  expect_true("isco08_title" %in% names(merged))
  expect_true("rti_mihaylov_2019" %in% names(merged))
  expect_true("greenness" %in% names(merged))
  expect_true("brownness" %in% names(merged))
})

test_that("load_isco prints citation message", {
  
  expect_message(base_isco(), "International Standard Classification of Occupations")
  expect_message(base_isco(), "ISCO.*2008")
  expect_message(base_isco(), "https://ilostat.ilo.org/methods/concepts-and-definitions/classification-occupation/")
  expect_message(base_isco(), "427 4-digit ISCO-08 occupation codes")
})
