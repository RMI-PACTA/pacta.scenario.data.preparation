test_that("output has expected columns", {
  out <- fake_format_p4b_input() %>%
    format_p4b()

  expected_names <- c(
    "scenario_source",
    "region",
    "scenario",
    "sector",
    "technology",
    "year",
    "smsp",
    "tmsr"
  )

  expect_equal(names(out), expected_names)
})

test_that("throws error when missing input columns", {
  data <- dplyr::select(fake_format_p4b_input(), -"source")

  expect_error(
    format_p4b(data),
    "Must have missing names"
  )
})
