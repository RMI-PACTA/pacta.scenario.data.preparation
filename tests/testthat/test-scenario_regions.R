test_that("exported data matches raw data", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "Not run in R CMD check")
  raw_data <-
    readr::read_csv(
      file = here::here("data-raw/scenario_regions.csv"),
      col_types = "ccci",
      na = ""
    )

  expect_equal(
    scenario_regions,
    raw_data
  )
})
