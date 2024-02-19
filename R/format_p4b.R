#' Format scenario data for P4B
#'
#' @param data A scenario dataset, like [pacta.scenario.preparation::weo_2021].

#'
#' @return A scenario dataset, with columns renamed to be consistent with
#'   r2dii.analysis target_market_share input requirements.
#'
#' @examples
#' library(dplyr)
#' weo_2022 %>%
#'   interpolate_yearly(source,scenario, sector, technology, scenario_geography, indicator, units) %>%
#'   filter(year >= 2022) %>%
#'   add_market_share_columns(reference_year = 2022) %>%
#'   format_p4b()
format_p4b <- function(data) {

  crucial_names <- c(
    "source",
    "scenario",
    "scenario_geography",
    "sector",
    "technology",
    "indicator",
    "units",
    "year",
    "tmsr",
    "smsp"
  )

  check_crucial_names(data, crucial_names)

  data <- data %>%
    mutate(
      scenario = tolower(.data$scenario),
      sector = tolower(.data$sector),
      technology = tolower(.data$technology),
      scenario_geography = tolower(.data$scenario_geography),
      tmsr = .data$tmsr + 1,
    ) %>%
    dplyr::left_join(pacta.scenario.preparation::dictionary_p4i_p4b, by = c(source = "p4i_label")) %>%
    mutate(sector = ifelse(.data$sector == "oil&gas", "oil and gas", .data$sector))

  data %>%
    dplyr::transmute(
      scenario_source = as.character(.data$p4b_label),
      region = as.character(.data$scenario_geography),
      .data$scenario,
      .data$sector,
      .data$technology,
      .data$year,
      .data$smsp,
      .data$tmsr
    )
}