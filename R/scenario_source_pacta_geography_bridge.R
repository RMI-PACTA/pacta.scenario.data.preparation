#' A dataset that maps scenario regions as defined by their source, to a list of
#' PACTA compatible scenario regions.
#'
#' @description
#' Scenario providers often define their own, tailor-made lists of what
#' countries form a region. The entire concept of a region is not standardized
#' and can even change year on year. However, for the purpose of the PACTA
#' transition monitor website, it is useful to have a minimal set of comparable
#' regions, to cycle through different scenarios for. Right now, these regions
#' are the "Global" region, which contains all countries, and the "OECD" and
#' "NonOECD" regions.
#'
#' This dataset provides a bridge between whatever the scenario has labelled
#' these regions as (e.g. "WORLD"), and the terminology that PACTA uses
#' (e.g. "Global").
#'
#' @examples
#' head(scenario_source_pacta_geography_bridge)
"scenario_source_pacta_geography_bridge"
