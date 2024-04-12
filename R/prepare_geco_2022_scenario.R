#' Prepare GECO 2022 scenario data
#'
#' @param geco_2022_automotive_raw A raw GECO 2022 automotive scenario
#'   data-frame.
#' @param geco_2022_aviation_raw A raw GECO 2022 aviation scenario data-frame.
#' @param geco_2022_fossil_fuels_15c_raw A raw GECO 2022 fossil fuels 1.5C
#'   scenario data-frame.
#' @param geco_2022_fossil_fuels_ndc_raw A raw GECO 2022 fossil fuels NDC
#'   scenario data-frame.
#' @param geco_2022_fossil_fuels_ref_raw A raw GECO 2022 fossil fuels reference
#'   scenario data-frame.
#' @param geco_2022_power_15c_raw A raw GECO 2022 power 1.5C scenario
#'   data-frame.
#' @param geco_2022_power_ndc_raw A raw GECO 2022 power NDC scenario data-frame.
#' @param geco_2022_power_ref_raw A raw GECO 2022 power reference scenario
#'   data-frame.
#' @param geco_2022_steel_raw A raw GECO 2022 steel scenario data-frame.
#'
#' @return A prepared GECO 2022 scenario data-frame.
#' @export
prepare_geco_2022_scenario <- function(geco_2022_automotive_raw,
                                       geco_2022_aviation_raw,
                                       geco_2022_fossil_fuels_15c_raw,
                                       geco_2022_fossil_fuels_ndc_raw,
                                       geco_2022_fossil_fuels_ref_raw,
                                       geco_2022_power_15c_raw,
                                       geco_2022_power_ndc_raw,
                                       geco_2022_power_ref_raw,
                                       geco_2022_steel_raw) {
  stopifnot(
    is.data.frame(geco_2022_automotive_raw),
    is.data.frame(geco_2022_aviation_raw),
    is.data.frame(geco_2022_fossil_fuels_15c_raw),
    is.data.frame(geco_2022_fossil_fuels_ndc_raw),
    is.data.frame(geco_2022_fossil_fuels_ref_raw),
    is.data.frame(geco_2022_power_15c_raw),
    is.data.frame(geco_2022_power_ndc_raw),
    is.data.frame(geco_2022_power_ref_raw),
    is.data.frame(geco_2022_steel_raw)
  )

  geco_2022_automotive <- prepare_geco_2022_automotive_scenario(
    geco_2022_automotive_raw
  )

  geco_2022_fossil_fuels <- prepare_geco_2022_fossil_fuels_scenario(
    geco_2022_fossil_fuels_15c_raw,
    geco_2022_fossil_fuels_ndc_raw,
    geco_2022_fossil_fuels_ref_raw
  )

  geco_2022_power <- prepare_geco_2022_power_scenario(
    geco_2022_power_15c_raw,
    geco_2022_power_ndc_raw,
    geco_2022_power_ref_raw
  )

  geco_2022_steel <- prepare_geco_2022_steel_scenario(geco_2022_steel_raw)

  geco_2022_aviation <- prepare_geco_2022_aviation_scenario(geco_2022_aviation_raw)

  geco_2022 <- dplyr::bind_rows(
    geco_2022_power,
    geco_2022_automotive,
    geco_2022_fossil_fuels,
    geco_2022_aviation,
    geco_2022_steel
  )

  geco_2022 <- dplyr::filter(geco_2022, .data$source == "GECO2022")

  geco_2022 <- dplyr::mutate(
    geco_2022,
    technology = gsub("oil", "Oil", .data$technology),
    technology = gsub("gas", "Gas", .data$technology),
    technology = gsub("coal", "Coal", .data$technology),
    technology = gsub("cap", "Cap", .data$technology),
    technology = gsub("hybrid", "Hybrid", .data$technology),
    technology = gsub("electric", "Electric", .data$technology),
    technology = gsub("ice", "ICE", .data$technology),
    technology = gsub("fuelcell", "FuelCell", .data$technology),
    technology = gsub("renewables", "Renewables", .data$technology),
    technology = gsub("hydro", "Hydro", .data$technology),
    technology = gsub("nuclear", "Nuclear", .data$technology),
    sector = gsub("oil&gas", "Oil&Gas", .data$sector),
    sector = gsub("coal", "Coal", .data$sector),
    sector = gsub("power", "Power", .data$sector),
    sector = gsub("automotive", "Automotive", .data$sector),
    sector = gsub("heavy-duty vehicles", "HDV", .data$sector)
  )

  geco_2022 <- dplyr::mutate(
    geco_2022,
    technology = ifelse(
      .data$sector == "hdv", paste0(.data$technology, "_hdv"), .data$technology
    )
  )

  geco_2022 <- bridge_geographies(geco_2022, geco_2022_geography_bridge)

  geco_2022 <- dplyr::mutate(
    geco_2022,
    scenario = dplyr::case_when(
      grepl(pattern = "1.5", x = .data$scenario) ~ "1.5C",
      grepl("NDC", .data$scenario) ~ "NDC_LTS",
      grepl("Ref", .data$scenario) ~ "Reference",
      TRUE ~ NA_character_
    )
  )

  if (any(is.na(unique(geco_2022$scenario)))) {
    rlang::abort("`NA` scenario names are not well-defined. Please review!")
  }

  out <- dplyr::select(geco_2022, prepared_scenario_names())

  # pacta.data.validation::validate_intermediate_scenario_output(out)
  out
}

prepare_geco_2022_automotive_scenario <- function(geco_2022_automotive_raw) {
  # TODO: currently still using retirement rates from geco2021
  # needs to be revisited, once we get an update
  out <- janitor::clean_names(geco_2022_automotive_raw)
  out <- bridge_technologies(out, geco_2022_technology_bridge)

  out <- tidyr::pivot_longer(
    out,
    cols = tidyr::matches("x20[0-9]{2}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  out <- dplyr::mutate(out, vehicle = NULL)

  out <- dplyr::rename(
    out,
    source = "geco",
    scenario_geography = "region",
    units = "unit",
    indicator = "variable"
  )

  out <- dplyr::summarise(
    out,
    value = sum(.data$value, na.rm = TRUE),
    .by = tidyr::all_of(scenario_summary_groups())
  )

  out <- dplyr::mutate(
    out,
    sector = ifelse(.data$sector == "Light vehicles", "Automotive", "HDV")
  )

  dplyr::select(out, prepared_scenario_names())
}

prepare_geco_2022_fossil_fuels_scenario <- function(geco_2022_fossil_fuels_15c_raw,
                                                    geco_2022_fossil_fuels_ndc_raw,
                                                    geco_2022_fossil_fuels_ref_raw) {
  out <- dplyr::bind_rows(
    geco_2022_fossil_fuels_15c_raw,
    geco_2022_fossil_fuels_ndc_raw,
    geco_2022_fossil_fuels_ref_raw
  )

  out <- janitor::clean_names(out)

  out <- dplyr::rename(
    out,
    source = "geco",
    scenario_geography = "region",
    technology = "fuel",
    units = "unit",
    indicator = "variable"
  )

  out <- dplyr::mutate(out, x1 = NULL)

  out <- dplyr::mutate(
    out,
    sector = ifelse(.data$technology == "Coal", "Coal", "Oil&Gas")
  )

  out <- bridge_technologies(out, geco_2022_technology_bridge)

  out <- tidyr::pivot_longer(
    out,
    cols = tidyr::matches("x20[0-9]{2}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  out <- dplyr::mutate(out, year = as.double(.data$year))
  dplyr::select(out, prepared_scenario_names())
}

prepare_geco_2022_power_scenario <- function(geco_2022_power_15c_raw,
                                             geco_2022_power_ndc_raw,
                                             geco_2022_power_ref_raw) {
  out <- dplyr::bind_rows(
    geco_2022_power_15c_raw,
    geco_2022_power_ndc_raw,
    geco_2022_power_ref_raw
  )

  out <- dplyr::filter(
    out,
    # actually those technology are already included in Coal/Gas/Biomass and capacities are actually double counted if we don't filter
    !.data$Technology %in% c("Coal with CCUS", "Gas with CCUS", "Biomass & Waste CCUS")
  )

  out <- janitor::clean_names(out)

  out <- dplyr::rename(
    out,
    source = "geco",
    scenario_geography = "region",
    units = "unit",
    indicator = "variable"
  )


  out <- dplyr::mutate(out, sector = "Power")

  out <- dplyr::mutate(
    out,
    technology = dplyr::case_when(
      .data$indicator == "Capacity" & grepl("Coal", .data$technology) ~ "CoalCap",
      .data$indicator == "Capacity" & grepl("Oil", .data$technology) ~ "OilCap",
      .data$indicator == "Capacity" & grepl("Gas", .data$technology) ~ "GasCap",
      .data$technology == "Other" ~ "RenewablesCap",
      TRUE ~ .data$technology
    )
  )

  out <- bridge_technologies(out, geco_2022_technology_bridge)

  out <- tidyr::pivot_longer(
    out,
    cols = tidyr::matches("x20[0-9]{2}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  out <- dplyr::mutate(
    # raw data is off by a magnitude of 1000. Provided capacity values are MW, but
    # unit displays GW. We fix by dividing by 1000 and thus keep ourr standardized
    # unit of GW power capacity
    out,
    value = .data$value / 1000
  )


  out <- dplyr::mutate(
    out,
    scenario_geography = dplyr::case_when(
      .data$scenario_geography == "United Kingdom" ~ "UK",
      .data$scenario_geography == "Mediterranean Middle-East" ~ "Mediteranean Middle East",
      .data$scenario_geography == "Tunisia, Morocco and Western Sahara" ~ "Morocco & Tunisia",
      .data$scenario_geography == "Algeria and Libya" ~ "Algeria & Libya",
      .data$scenario_geography == "Rest of Central America and Caribbean" ~ "Rest Central America",
      .data$scenario_geography == "Rest of Balkans" ~ "Others Balkans",
      .data$scenario_geography == "Rest of Persian Gulf" ~ "Rest Gulf",
      .data$scenario_geography == "Rest of Pacific" ~ "Rest Pacific",
      .data$scenario_geography == "Rest of Sub-Saharan Africa" ~ "Rest Sub Saharan Africa",
      .data$scenario_geography == "Rest of South America" ~ "Rest South America",
      .data$scenario_geography == "Rest of South Asia" ~ "Rest South Asia",
      .data$scenario_geography == "Rest of South-East Asia" ~ "Rest South East Asia",
      .data$scenario_geography == "Russian Federation" ~ "Russia",
      .data$scenario_geography == "Saudi Arabia" ~ "SaudiArabia",
      .data$scenario_geography == "United States" ~ "US",
      .data$scenario_geography == "South Africa" ~ "SouthAfrica",
      .data$scenario_geography == "European Union" ~ "EU",
      .data$scenario_geography == "Korea (Republic)" ~ "South Korea",
      .data$scenario_geography == "Rest of CIS" ~ "Other CIS",
      TRUE ~ .data$scenario_geography
    )
  )

  out <- dplyr::filter(
    out,
    .data$scenario_geography != "Switzerland",
    .data$scenario_geography != "Iceland",
    .data$scenario_geography != "Norway"
  )

  out <- dplyr::summarise(
    out,
    value = sum(.data$value, na.rm = TRUE),
    .by = tidyr::all_of(scenario_summary_groups())
  )

  dplyr::select(out, prepared_scenario_names())
}

prepare_geco_2022_steel_scenario <- function(geco_2022_steel_raw) {
  out <- janitor::clean_names(geco_2022_steel_raw)

  out <- dplyr::rename(out, scenario_geography = "region")

  out <- dplyr::mutate(
    out,
    units = "tCO2/t Steel",
    indicator = "Emission Intensity",
    technology = NA_character_
  )

  out <- tidyr::pivot_longer(
    out,
    cols = tidyr::matches("x[0-9]{4}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  out <- dplyr::mutate(out, year = as.double(.data$year))

  dplyr::select(out, prepared_scenario_names())
}

prepare_geco_2022_aviation_scenario <- function(geco_2022_aviation_raw) {
  out <- dplyr::rename(
    geco_2022_aviation_raw,
    source = "GECO",
    scenario = "Scenario",
    indicator = "Variable",
    passenger_freight = "Passenger/Freight",
    technology = "Technology",
    units = "Unit",
    scenario_geography = "Region"
  )

  out <- dplyr::mutate(out, passenger_freight = NULL)

  out <- dplyr::mutate(
    out,
    sector = "Aviation",
    indicator = stringr::str_to_title(.data$indicator)
  )

  out <- tidyr::pivot_longer(
    out,
    cols = tidyr::matches("[0-9]{4}$"),
    names_to = "year",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  out <- dplyr::mutate(out, year = as.double(.data$year))

  dplyr::select(out, prepared_scenario_names())
}
