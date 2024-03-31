#' Prepare ISF 2023 scenario data
#'
#' @param isf_2023_scope_global_raw A tidyxl data frame (with a `formats`
#'   attribute) with a raw ISF Scope Global 2023 import.
#' @param isf_2023_annex_countries_raw A list of tidyxl data frames (with a `formats` attribute) containing the raw import of each of the Annex Countries xlsx files for ISF 2023.
#'
#' @return A prepared ISF 2023 scenario data-frame.
#'
#' @importFrom dplyr %>%
#'
#' @export

prepare_isf_2023_scenario <- function(isf_2023_scope_global_raw,
                                      isf_2023_annex_countries_raw) {
  isf_2023_power <-
    isf_2023_annex_countries_raw %>%
    purrr::map(extract_installed_capacity) %>%
    purrr::list_rbind()

  isf_2023_fossil_fuels <-
    isf_2023_annex_countries_raw %>%
    purrr::map(extract_final_energy_demand) %>%
    purrr::list_rbind()

  out <-
    dplyr::bind_rows(
      # final_steel_cement,
      isf_2023_fossil_fuels,
      isf_2023_power
    )

  out %>%
    bridge_technologies(isf_2023_technology_bridge) %>%
    dplyr::summarize(
      value = sum(.data[["value"]], na.rm = TRUE),
      .by = c(
        "source",
        "scenario",
        "scenario_geography",
        "sector",
        "technology",
        "indicator",
        "units",
        "year"
      )
    ) %>%
    dplyr::arrange(
      .data[["scenario_geography"]],
      .data[["sector"]],
      .data[["technology"]],
      .data[["year"]]
    ) %>%
    dplyr::relocate(
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


extract_installed_capacity <- function(x) {
  formats <- attr(x, "formats")
  top_border <- !is.na(formats$local$border$top$style)
  left_border <- !is.na(formats$local$border$left$style)

  corners <-
    x %>%
    dplyr::filter(top_border[.data[["local_format_id"]]] &
                    left_border[.data[["local_format_id"]]]) %>%
    dplyr::filter(col == 17)

  partitions <- unpivotr::partition(x, corners)
  tables <- dplyr::pull(partitions, "cells")

  tables[[1]] %>%
    dplyr::filter(!.data[["is_blank"]]) %>%
    unpivotr::behead("up-left", "table") %>%
    unpivotr::behead("up", "year", formatters = list(numeric = as.double)) %>%
    unpivotr::behead("left", "technology") %>%
    dplyr::mutate(technology = trimws(.data[["technology"]])) %>%
    dplyr::mutate(scenario_geography = sub("^Annex_", "", .data[["sheet"]])) %>%
    dplyr::mutate(scenario_geography = sub("Italy2", "Italy", .data[["scenario_geography"]])) %>%
    dplyr::select("technology", "scenario_geography", "year", value = "numeric") %>%
    dplyr::filter(
      !.data[["technology"]] %in% c(
        # remove double counting
        "Total generation",
        "- Fossil",
        "- Hydrogen (fuel cells, gas power plants, gas CHP)",
        "of which wind offshore",
        "- Renewables",
        "Variable RES (PV, Wind, Ocean)",
        "Share of variable RES",
        "RES share (domestic generation)"
      )
    ) %>%
    dplyr::mutate(
      sector = "Power",
      source = "ISF2023",
      scenario = "1.5\xc2\xb0C",
      units = "GW",
      indicator = "Capacity"
    ) %>%
    dplyr::arrange(.data[["scenario_geography"]], .data[["technology"]], .data[["year"]])
}


extract_final_energy_demand <- function(x) {
  formats <- attr(x, "formats")
  top_border <- !is.na(formats$local$border$top$style)
  left_border <- !is.na(formats$local$border$left$style)

  corners <-
    x %>%
    dplyr::filter(top_border[.data[["local_format_id"]]] &
                    left_border[.data[["local_format_id"]]]) %>%
    dplyr::filter(col == 17)

  partitions <- unpivotr::partition(x, corners)
  tables <- dplyr::pull(partitions, "cells")

  tables[[2]] %>%
    dplyr::filter(!.data[["is_blank"]]) %>%
    dplyr::filter(col <= 30) %>%
    unpivotr::behead("up-left", "table") %>%
    unpivotr::behead("up", "year", formatters = list(numeric = as.double)) %>%
    unpivotr::behead("left", "technology") %>%
    dplyr::mutate(technology = trimws(.data[["technology"]])) %>%
    dplyr::mutate(scenario_geography = sub("^Annex_", "", .data[["sheet"]])) %>%
    dplyr::mutate(scenario_geography = sub("Italy2", "Italy", .data[["scenario_geography"]])) %>%
    dplyr::select("technology", "scenario_geography", "year", value = "numeric") %>%
    dplyr::filter(
      .data[["technology"]] %in% c(
        # remove double counting
        # "- Oil products",
        "- Natural gas",
        # "- Hard coal & lignite",
        "- Gas",
        "- Oil",
        "- Coal"
      )
    ) %>%
    dplyr::mutate(
      sector = dplyr::case_when(
        technology %in% c("- Hard coal & lignite", "- Coal") ~ "Coal",
        technology %in% c(
          "- Oil products",
          "- Natural gas",
          "- Gas",
          "- Oil"
        ) ~ "Oil&Gas",
        TRUE ~ "Error"
      ),
      source = "ISF2023",
      scenario = "1.5\xc2\xb0C",
      units = "PJ",
      indicator = "Final Energy Demand"
    ) %>%
    dplyr::arrange(.data[["scenario_geography"]], .data[["technology"]], .data[["year"]]) %>%
    # We use Final Energy Demand using the assumption Demand = Production (i.e. no
    # stocks) - but this is not true at regional level because EU can increase its
    # demand in FF, it will be extracted elsewhere
    dplyr::filter(.data[["scenario_geography"]] == "Global") %>%
    # standardize fossil fuel units. conversion of units based on:
    # https://www.iea.org/data-and-statistics/data-tools/unit-converter
    dplyr::mutate(
      value = dplyr::if_else(
        .data$sector %in% c("Coal", "Oil&Gas") & .data$units == "PJ",
        .data$value * 0.02388,
        .data$value
      ),
      units = dplyr::if_else(
        .data$sector %in% c("Coal", "Oil&Gas") & .data$units == "PJ",
        "mtoe",
        .data$units
      ),
      indicator = dplyr::if_else(
        .data$sector %in% c("Coal", "Oil&Gas") & .data$indicator == "Final Energy Demand",
        "Demand",
        .data$indicator
      )
    )
}
