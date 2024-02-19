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
      glue(
        "Data must contain reference year: {reference_year}."),
      class = "missing_year"
    )
  }

  data <- dplyr::filter(data, data$year >= reference_year)

  data %>%
    add_technology_fair_share_ratio() %>%
    add_market_fair_share_percentage() %>%
    dplyr::group_by(!!!old_groups)
}

add_technology_fair_share_ratio <- function(data) {
  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(tmsr = (.data$value - dplyr::first(.data$value)) / dplyr::first(.data$value)) %>%
    # if both the first value and the value at time t are 0, the resulting TMSR is NaN.
    # to avoid issues in calculations later on, we set tmsr = 0 in that case,
    # which implies no change. Note that this will be returned as a ratio of 1,
    # based on format_p4b(), but the tmsr is a plain rate of change at this point
    dplyr::mutate(
      tmsr = dplyr::if_else(
        .data$value == 0 & dplyr::first(.data$value) == 0,
        0,
        .data$tmsr
      )
    ) %>%
    dplyr::ungroup()
}

add_market_fair_share_percentage <- function(data) {
  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "year"))) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(sector_total_by_year = sum(.data$value)) %>%
    dplyr::group_by(!!!syms(c(common_fs_groups(), "technology"))) %>%
    dplyr::mutate(
      smsp = (.data$value - dplyr::first(.data$value)) /
        dplyr::first(.data$sector_total_by_year),
      sector_total_by_year = NULL
    ) %>%
    dplyr::ungroup()
}

common_fs_groups <- function() {
  c("scenario", "sector", "scenario_geography")
}
