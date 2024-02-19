#' Add market share columns to a scenario dataset
#'
#' Calculates and adds market share values (ie. technology market-share ratio
#' and sector market-share percentage) to a scenario dataset. A reference
#' start-year must be provided.
#'
#' @param data A scenario dataset, like FIXME: Define an exported demo scenario.
#' @param reference_year The baseline year, against which the technology- and
#'   sector- market shares will be calculated. Note: At the start year, tmsr = 1
#'   and smsp =0 respectively.
#'
#' @return A scenario dataset, with the new columns `tmsr` and `smsp`.
add_market_share_columns <- function(data, reference_year) {
  old_groups <- dplyr::groups(data)
  data <- dplyr::ungroup(data)

  if (!(reference_year %in% data$year)) {
    rlang::abort(
      glue::glue(
        "Data must contain reference year: {reference_year}."
      ),
      class = "missing_year"
    )
  }

  data <- dplyr::filter(data, data$year >= reference_year)
  data <- add_technology_fair_share_ratio(data)
  data <- add_market_fair_share_percentage(data)

  dplyr::group_by(data, !!!old_groups)
}

add_technology_fair_share_ratio <- function(data) {
  data <- dplyr::ungroup(data)
  data <- dplyr::group_by(data, !!!syms(c(common_fs_groups(), "technology")))
  data <- dplyr::arrange(data, data$year, .by_group = TRUE)
  data <- dplyr::mutate(data, tmsr = (data$value - dplyr::first(data$value)) / dplyr::first(data$value))

  # if both the first value and the value at time t are 0, the resulting TMSR is NaN.
  # to avoid issues in calculations later on, we set tmsr = 0 in that case,
  # which implies no change. Note that this will be returned as a ratio of 1,
  # based on format_p4b(), but the tmsr is a plain rate of change at this point
  data <- dplyr::mutate(data, tmsr = ifelse(data$value == 0 & dplyr::first(data$value) == 0, 0, data$tmsr))

  dplyr::ungroup(data)
}

add_market_fair_share_percentage <- function(data) {
  data <- dplyr::ungroup(data)
  data <- dplyr::group_by(data, !!!syms(c(common_fs_groups(), "year")))
  data <- dplyr::arrange(data, data$year, .by_group = TRUE)
  data <- dplyr::mutate(data, sector_total_by_year = sum(data$value))
  data <- dplyr::group_by(data, !!!syms(c(common_fs_groups(), "technology")))
  data <- dplyr::mutate(data, smsp = (data$value - dplyr::first(data$value)) / dplyr::first(data$sector_total_by_year))

  dplyr::ungroup(data)
}

common_fs_groups <- function() {
  c("scenario", "sector", "scenario_geography")
}
