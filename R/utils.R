prepared_scenario_names <- function() {
  c(
    scenario_summary_groups(),
    "value"
  )
}

scenario_summary_groups <- function() {
  c(
    "source",
    "sector",
    "scenario_geography",
    "scenario",
    "indicator",
    "units",
    "technology",
    "year"
  )
}

dictionary_p4i_p4b <- function() {
  dplyr::tibble(
    p4i_label = c("WEO2022", "GECO2022"),
    p4b_label = c("weo_2022", "geco_2022")
  )
}
