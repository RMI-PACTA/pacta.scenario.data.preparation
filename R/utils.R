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

bridge_geographies <- function(data, geography_bridge) {
  out <- dplyr::left_join(
    data,
    geography_bridge,
    by = c("scenario_geography" = "scenario_geography_name")
  )

  dplyr::mutate(
    out,
    scenario_geography = .data[["standardized_geography_name"]],
    .keep = "unused"
  )
}

bridge_technologies <- function(data, technology_bridge) {
  out <- dplyr::left_join(
    data,
    technology_bridge,
    by = c("technology" = "scenario_technology_name")
  )

  out <- dplyr::mutate(out, technology = NULL)
  dplyr::rename(out, technology = "standardized_technology_name")
}

dictionary_p4i_p4b <- function() {
  dplyr::tribble(
    ~p4i_label, ~p4b_label,
    "ISF2021", "isf_2021",
    "WEO2022", "weo_2022",
    "GECO2022", "geco_2022",
    "GECO2023", "geco_2023",
    "WEO2023", "weo_2023",
    "ISF2023", "isf_2023",
    "WEO2024", "weo_2024"
  )
}
