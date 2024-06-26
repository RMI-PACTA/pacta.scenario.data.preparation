#' Format scenario data for P4I
#'
#' @param data A scenario dataset.
#' @param green_techs A list of green technologies. For these, a `direction` of
#'   "increasing" will be assigned, and the `smsp` column will be used to assign
#'   a `fair_share_perc`. Otherwise the `direction` will be `decreasing` and the
#'   `tmsr` column will be used.
#'
#' @return A scenario dataset, with columns renamed to be consistent with
#'   pacta.data.preparation input requirements.
#'
#' @export

format_p4i <- function(data, green_techs) {
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
    direction = dplyr::if_else(.data$technology %in% .env$green_techs, "increasing", "declining"),
    fair_share_perc = dplyr::if_else(.data$direction == "declining", .data$tmsr, .data$smsp)
  )

  data <- dplyr::mutate(
    data,
    technology = dplyr::if_else(
      .data$sector == "HDV",
      paste0(.data$technology, "_", .data$sector),
      .data$technology
    ),
    sector = dplyr::if_else(
      .data$sector == "HDV",
      "Automotive",
      .data$sector
    )
  )

  dplyr::transmute(
    data,
    scenario_source = as.character(.data$source),
    .data$scenario_geography,
    .data$scenario,
    ald_sector = as.character(.data$sector),
    .data$technology,
    .data$units,
    .data$year,
    .data$direction,
    .data$fair_share_perc
  )
}
