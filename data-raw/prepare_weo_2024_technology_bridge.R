weo_2024_technology_bridge <-
  readr::read_csv(
    file = "data-raw/weo_2024_technology_bridge.csv",
    na = "",
    col_types = readr::cols(
      scenario_technology_name = "c",
      standardized_technology_name = "c"
    )
  )
