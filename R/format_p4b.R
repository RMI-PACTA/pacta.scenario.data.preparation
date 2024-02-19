#' Format scenario data for P4B
#'
#' @param data A scenario dataset.

#'
#' @return A scenario dataset, with columns renamed to be consistent with
#'   r2dii.analysis target_market_share input requirements.
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

  data <- dplyr::mutate(
    data,
    scenario = tolower(.data$scenario),
    sector = tolower(.data$sector),
    technology = tolower(.data$technology),
    scenario_geography = tolower(.data$scenario_geography),
    tmsr = .data$tmsr + 1,
  )

  data <- dplyr::left_join(data, dictionary_p4i_p4b(), by = c(source = "p4i_label"))
  data <- dplyr::mutate(sector = ifelse(.data$sector == "oil&gas", "oil and gas", .data$sector))

  dplyr::transmute(
    data,
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
