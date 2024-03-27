test_that("exported data matches raw data", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "Not run in R CMD check")
  raw_data <-
    readr::read_csv(
      file = here::here("data-raw/geco_2023_technology_bridge.csv"),
      na = "",
      col_types = readr::cols(
        scenario_technology_name = "c",
        standardized_technology_name = "c"
      )
    )

  expect_equal(
    pacta.scenario.data.preparation::geco_2023_technology_bridge,
    raw_data
  )
})
