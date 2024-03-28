geco_2022_geography_bridge <-
  readr::read_csv(
    file = "data-raw/geco_2022_geography_bridge.csv",
    na = "",
    col_types = readr::cols(
      scenario_geography_name = "c",
      standardized_geography_name = "c"
    )
  )
