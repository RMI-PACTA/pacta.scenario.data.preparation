geco_2023_technology_bridge <-
  readr::read_csv(
    file = "data-raw/geco_2023_technology_bridge.csv",
    na = "",
    col_types = readr::cols(
      scenario_technology_name = "c",
      standardized_technology_name = "c"
    )
  )

usethis::use_data(
  geco_2023_technology_bridge,
  overwrite = TRUE
)
