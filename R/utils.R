prepared_scenario_names <- function() {
  c(scenario_summary_groups(), "value")
}

scenario_summary_groups <- function() {
  c(
    "source",
    "scenario",
    "scenario_geography",
    "sector",
    "technology",
    "indicator",
    "units",
    "year"
  )
}

bridge_technologies <- function(data, technology_bridge) {
  out <- dplyr::left_join(
    data,
    technology_bridge,
    by = c("technology" = "TechnologyAll")
  )

  out <- dplyr::mutate(out, technology = NULL)
  dplyr::rename(out, technology = .data$TechnologyName)
}

dictionary_p4i_p4b <- function() {
  dplyr::tibble(
    p4i_label = c("WEO2022", "GECO2022"),
    p4b_label = c("weo_2022", "geco_2022")
  )
}
