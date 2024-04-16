#' Prepare ISF 2021 scenario data
#'
#' @param isf_2021_power_raw A tidyxl data frame containing a raw import of
#'   `NZAOA_raw_data_power.xlsx`.
#' @param isf_2021_not_power_raw A tidyxl data frame containing a raw import of
#'   `NZAOA_rawdata_notpower_P4I.xlsx`.
#'
#' @return A prepared ISF 2021 scenario data-frame.
#'
#' @importFrom dplyr %>%
#'
#' @export

prepare_isf_2021_scenario <- function(isf_2021_power_raw,
                                      isf_2021_not_power_raw) {

  isf_power_raw <-
    isf_2021_power_raw %>%
    dplyr::filter(.data[["sheet"]] == "Sheet2") %>%
    unpivotr::behead("up", "year") %>%
    unpivotr::behead("left", "Source") %>%
    unpivotr::behead("left", "Indicator") %>%
    unpivotr::behead("left", "Sector") %>%
    unpivotr::behead("left", "Units") %>%
    unpivotr::behead("left", "Scenario") %>%
    unpivotr::behead("left", "ScenarioGeography") %>%
    unpivotr::behead("left", "Technology") %>%
    unpivotr::behead("right", "SourceSheet") %>%
    dplyr::select(
      "Source",
      "Indicator",
      "Sector",
      "Units",
      "Scenario",
      "ScenarioGeography",
      "Technology",
      "SourceSheet",
      "year",
      value = "numeric"
    ) %>%
    dplyr::mutate(
      year = as.numeric(.data[["year"]]),
      value = as.numeric(.data[["value"]])
    )

  isf_not_power_raw <-
    isf_2021_not_power_raw %>%
    dplyr::filter(.data[["sheet"]] == "NZAOA_rawdata_notpower") %>%
    unpivotr::behead("up", "year") %>%
    unpivotr::behead("left", "Source") %>%
    unpivotr::behead("left", "Indicator") %>%
    unpivotr::behead("left", "Sector") %>%
    unpivotr::behead("left", "Units") %>%
    unpivotr::behead("left", "Scenario") %>%
    unpivotr::behead("left", "ScenarioGeography") %>%
    unpivotr::behead("left", "Technology") %>%
    unpivotr::behead("right", "SourceSheet") %>%
    dplyr::select(
      "Source",
      "Indicator",
      "Sector",
      "Units",
      "Scenario",
      "ScenarioGeography",
      "Technology",
      "SourceSheet",
      "year",
      value ="character"
    ) %>%
    dplyr::mutate(
      year = as.numeric(.data[["year"]]),
      value = as.numeric(.data[["value"]])
    )

  out <-
    dplyr::bind_rows(isf_power_raw, isf_not_power_raw) %>%
    janitor::clean_names() %>%
    dplyr::summarise(
      value = sum(.data[["value"]]),
      .by = -"source_sheet"
    ) %>%
    bridge_geographies(isf_2021_geography_bridge) %>%
    bridge_technologies(isf_2021_technology_bridge) %>%
    dplyr::mutate(
      source = "ISF2021",
      scenario = "NZE"
    ) %>%
    # standardize technologies
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$sector == "Cement" ~ NA_character_,
        .data$sector == "Steel" ~ NA_character_,
        TRUE ~ .data$technology
      )
    ) %>%
    # conversion of units based on https://www.iea.org/data-and-statistics/data-tools/unit-converter
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
      )
    ) %>%
    # standardize units
    dplyr::mutate(
      units = dplyr::case_when(
        .data$sector == "Cement" ~ "tCO2/t Cement",
        .data$sector == "Steel" ~ "tCO2/t Steel",
        TRUE ~ .data$units
      )
    ) %>%
    # Cement and Steel sector values not sufficiently validated from ISF, so we drop them for now
    dplyr::filter(!.data[["sector"]] %in% c("Cement", "Steel")) %>%
    dplyr::filter(.data[["sector"]] != "Aviation") %>%
    dplyr::select(
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

  pacta.data.validation::validate_intermediate_scenario_output(out)

  out
}
