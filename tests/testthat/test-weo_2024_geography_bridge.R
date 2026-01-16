test_that("exported data matches raw data", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "Not run in R CMD check")
  raw_data <-
    readr::read_csv(
      file = here::here("data-raw/weo_2024_geography_bridge.csv"),
      na = "",
      col_types = readr::cols(
        scenario_geography_name = "c",
        standardized_geography_name = "c"
      )
    )

  expect_equal(
    weo_2024_geography_bridge,
    raw_data
  )
})
