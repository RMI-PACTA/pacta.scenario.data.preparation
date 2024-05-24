#' Format scenario data for P4B
#'
#' @param data A scenario data-frame following the format created in the
#'   prepare_*.R scripts.
#'
#' @return A scenario data-frame with columns renamed to be consistent with
#'   r2dii.analysis target_sda input requirements.
#'
#' @importFrom dplyr %>%
#'
#' @export

format_p4b_ei <- function(data) {
  crucial_names <- c(
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

  check_crucial_names(data, crucial_names)

  data <- dplyr::mutate(
    data,
    scenario = tolower(.data$scenario),
    sector = tolower(.data$sector),
    technology = tolower(.data$technology),
    scenario_geography = tolower(.data$scenario_geography)
  )

  data <- dplyr::mutate(
    data,
    emission_factor_unit = dplyr::case_when(
      .data$sector == "cement" ~ "tonnes of CO2 per tonne of cement",
      .data$sector == "aviation" ~ "tonnes of CO2 per passenger per km travelled",
      .data$sector == "steel" ~ "tonnes of CO2 per tonne of steel",
      TRUE ~ .data$sector
    )
  )

  data <- dplyr::left_join(
    data,
    dictionary_p4i_p4b(),
    by = c(source = "p4i_label")
  )

  data <- dplyr::mutate(
    data,
    sector = dplyr::if_else(
      .data$sector == "oil&gas",
      "oil and gas",
      .data$sector
    )
  )

  data <- dplyr::transmute(
    data,
    scenario_source = as.character(.data$p4b_label),
    region = as.character(.data$scenario_geography),
    .data$scenario,
    .data$sector,
    .data$year,
    emission_factor = as.double(.data$value),
    .data$emission_factor_unit
  )
}
