test_that("output has expected columns", {
  out <- fake_format_p4b_ei_input() %>%
    format_p4b_ei()

  expected_names <- c(
    "scenario_source",
    "region",
    "scenario",
    "sector",
    "year",
    "emission_factor",
    "emission_factor_unit"
  )

  expect_equal(names(out), expected_names)
})

test_that("throws error when missing input columns", {
  data <- dplyr::select(fake_format_p4b_ei_input(), -"source")

  expect_error(
    format_p4b_ei(data),
    "Must have missing names"
  )
})
