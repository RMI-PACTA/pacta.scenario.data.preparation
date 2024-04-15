test_that("exported data matches raw data", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "Not run in R CMD check")
  raw_data <-
    readr::read_csv(
      file = here::here("data-raw/scenario_source_pacta_geography_bridge.csv"),
      col_types = "ccc",
      na = ""
    )

  expect_equal(
    scenario_source_pacta_geography_bridge,
    raw_data
  )
})
