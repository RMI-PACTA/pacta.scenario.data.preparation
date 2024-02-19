standardized_scenario_columns <- function() {
  c(
    "source",
    "scenario",
    "scenario_geography",
    "sector",
    "technology",
    "indicator",
    "units",
    "year",
    "value"
  )
}

dictionary_p4i_p4b <- function() {
  dplyr::tibble(
    p4i_label = c("WEO2022", "GECO2022"),
    p4b_label = c("weo_2022", "geco_2022")
  )
}
