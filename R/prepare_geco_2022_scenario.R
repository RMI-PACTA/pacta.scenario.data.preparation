#' Prepare GECO 2022 scenario data
#'
#' @param technology_bridge A technology bridge data-frame.
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
prepare_geco_2022_scenario <- function(technology_bridge,
                                       geco_2022_automotive_raw,
                                       geco_2022_aviation_raw,
                                       geco_2022_fossil_fuels_15c_raw,
                                       geco_2022_fossil_fuels_ndc_raw,
                                       geco_2022_fossil_fuels_ref_raw,
                                       geco_2022_power_15c_raw,
                                       geco_2022_power_ndc_raw,
                                       geco_2022_power_ref_raw,
                                       geco_2022_steel_raw) {
  geco_2022_automotive <- prepare_geco_2022_automotive_scenario(
    technology_bridge,
    geco_2022_automotive_raw
  )

  geco_2022_fossil_fuels <- prepare_geco_2022_fossil_fuels_scenario(
    technology_bridge,
    geco_2022_fossil_fuels_15c_raw,
    geco_2022_fossil_fuels_ndc_raw,
    geco_2022_fossil_fuels_ref_raw
  )


  # format power -----------------------------------------------------------------
  geco_2022_power <- dplyr::bind_rows(
    geco_2022_power_15c_raw,
    geco_2022_power_ndc_raw,
    geco_2022_power_ref_raw
  )

  geco_2022_power <- dplyr::filter(
    geco_2022_power,
    # actually those technology are already included in Coal/Gas/Biomass and capacities are actually double counted if we don't filter
    !.data$Technology %in% c("Coal with CCUS", "Gas with CCUS", "Biomass & Waste CCUS")
  )

  geco_2022_power <- janitor::clean_names(geco_2022_power)

  geco_2022_power <- dplyr::rename(
    geco_2022_power,
    source = "geco",
    scenario_geography = "region",
    units = "unit",
    indicator = "variable"
  )


  geco_2022_power <- dplyr::mutate(
    geco_2022_power,
    sector = "Power"
  )

  geco_2022_power <- dplyr::mutate(
    geco_2022_power,
    technology = dplyr::case_when(
      .data$indicator == "Capacity" & grepl("Coal", .data$technology) ~ "CoalCap",
      .data$indicator == "Capacity" & grepl("Oil", .data$technology) ~ "OilCap",
      .data$indicator == "Capacity" & grepl("Gas", .data$technology) ~ "GasCap",
      .data$technology == "Other" ~ "RenewablesCap",
      TRUE ~ .data$technology
    )
  )

  geco_2022_power <- dplyr::left_join(geco_2022_power, technology_bridge, by = c("technology" = "TechnologyAll"))
  geco_2022_power <- dplyr::mutate(geco_2022_power, technology = NULL)
  geco_2022_power <- dplyr::rename(geco_2022_power, technology = "TechnologyName")


  geco_2022_power <- tidyr::pivot_longer(
    geco_2022_power,
    cols = tidyr::matches("x20[0-9]{2}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  geco_2022_power <- dplyr::mutate(
    # raw data is off by a magnitude of 1000. Provided capacity values are MW, but
    # unit displays GW. We fix by dividing by 1000 and thus keep ourr standardized
    # unit of GW power capacity
    geco_2022_power,
    value = .data$value / 1000
  )


  geco_2022_power <- dplyr::mutate(
    geco_2022_power,
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

  geco_2022_power <- dplyr::filter(
    geco_2022_power,
    .data$scenario_geography != "Switzerland",
    .data$scenario_geography != "Iceland",
    .data$scenario_geography != "Norway"
  )

  geco_2022_power <- dplyr::summarise(
    geco_2022_power,
    value = sum(.data$value, na.rm = TRUE),
    .by = tidyr::all_of(scenario_summary_groups())
  )


  # format steel ------------------------------------------------------------
  geco_2022_steel <- janitor::clean_names(geco_2022_steel_raw)

  geco_2022_steel <- dplyr::rename(geco_2022_steel, scenario_geography = "region")

  geco_2022_steel <- dplyr::mutate(
    geco_2022_steel,
    units = "tCO2/t Steel",
    indicator = "Emission Intensity",
    technology = NA_character_
  )

  geco_2022_steel <- tidyr::pivot_longer(
    geco_2022_steel,
    cols = tidyr::matches("x[0-9]{4}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  geco_2022_steel <- dplyr::relocate(
    geco_2022_steel,
    c("source", "scenario", "indicator", "sector", "technology", "units", "scenario_geography", "year", "value")
  )

  geco_2022_steel <- dplyr::mutate(geco_2022_steel, year = as.double(.data$year))

  # format aviation ------------------------------------------------------------
  geco_2022_aviation <- janitor::clean_names(geco_2022_aviation_raw)

  geco_2022_aviation <- dplyr::rename(
    geco_2022_aviation,
    source = "geco",
    scenario_geography = "region",
    units = "unit",
    indicator = "variable"
  )

  geco_2022_aviation <- dplyr::mutate(
    geco_2022_aviation,
    passenger_freight = NULL
  )


  geco_2022_aviation <- dplyr::mutate(
    geco_2022_aviation,
    sector = "Aviation",
    indicator = stringr::str_to_title(.data$indicator)
  )

  geco_2022_aviation <- tidyr::pivot_longer(
    geco_2022_aviation,
    cols = tidyr::matches("x[0-9]{4}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  geco_2022_aviation <- dplyr::relocate(
    geco_2022_aviation,
    c("source", "scenario", "indicator", "sector", "technology", "units", "scenario_geography", "year", "value")
  )

  geco_2022_aviation <- dplyr::mutate(
    geco_2022_aviation,
    year = as.double(.data$year)
  )


  # combine and format ------------------------------------------------------
  geco_2022 <- rbind(
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

  geco_2022 <- dplyr::mutate(
    geco_2022,
    scenario_geography = dplyr::case_when(
      .data$scenario_geography == "NOAP" ~ "Algeria & Libya",
      .data$scenario_geography == "MEME" ~ "Mediteranean Middle East",
      .data$scenario_geography == "NOAN" ~ "Morocco & Tunisia",
      .data$scenario_geography == "NZL" ~ "New Zealand",
      .data$scenario_geography == "RCIS" ~ "Other CIS",
      .data$scenario_geography == "RCAM" ~ "Rest Central America",
      .data$scenario_geography == "RCEU" ~ "Other Balkan",
      .data$scenario_geography == "RSAM" ~ "Rest South America",
      .data$scenario_geography == "RSAS" ~ "Rest South Asia",
      .data$scenario_geography == "RSEA" ~ "Rest South East Asia",
      .data$scenario_geography == "RSAF" ~ "Rest Sub Saharan Africa",
      .data$scenario_geography == "RGLF" ~ "Rest Gulf",
      .data$scenario_geography == "RPAC" ~ "Rest Pacific",
      .data$scenario_geography == "KOR" ~ "South Korea",
      .data$scenario_geography == "World" ~ "Global",
      .data$scenario_geography == "THA" ~ "Thailand",
      .data$scenario_geography == "EU" ~ "EU27",
      .data$scenario_geography == "NOR" ~ "Norway",
      .data$scenario_geography == "ISL" ~ "Iceland",
      .data$scenario_geography == "CHE" ~ "Switzerland",
      .data$scenario_geography == "TUR" ~ "Turkey",
      .data$scenario_geography == "RUS" ~ "Russia",
      .data$scenario_geography == "USA" ~ "US",
      .data$scenario_geography == "CAN" ~ "Canada",
      .data$scenario_geography == "BRA" ~ "Brazil",
      .data$scenario_geography == "ARG" ~ "Argentina",
      .data$scenario_geography == "CHL" ~ "Chile",
      .data$scenario_geography == "AUS" ~ "Australia",
      .data$scenario_geography == "JPN" ~ "Japan",
      .data$scenario_geography == "CHN" ~ "China",
      .data$scenario_geography == "IND" ~ "India",
      .data$scenario_geography == "SAU" ~ "Saudi Arabia",
      .data$scenario_geography == "IRN" ~ "Iran",
      .data$scenario_geography == "EGY" ~ "Egypt",
      .data$scenario_geography == "ZAF" ~ "South Africa",
      .data$scenario_geography == "MEX" ~ "Mexico",
      .data$scenario_geography == "IDN" ~ "Indonesia",
      .data$scenario_geography == "UKR" ~ "Ukraine",
      .data$scenario_geography == "MYS" ~ "Malaysia",
      .data$scenario_geography == "VNM" ~ "Vietnam",
      .data$scenario_geography == "GBR" ~ "UK",
      TRUE ~ .data$scenario_geography
    )
  )


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

  dplyr::select(
    geco_2022,
    prepared_scenario_names()
  )
}

prepare_geco_2022_automotive_scenario <- function(technology_bridge,
                                                  geco_2022_automotive_raw) {
  # TODO: currently still using retirement rates from geco2021
  # needs to be revisited, once we get an update
  out <- janitor::clean_names(geco_2022_automotive_raw)
  out <- bridge_technologies(out, technology_bridge)

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

  dplyr::mutate(
    out,
    sector = ifelse(
      .data$sector == "Light vehicles", "Automotive", "HDV"
    )
  )
}

prepare_geco_2022_fossil_fuels_scenario <- function(technology_bridge,
                                                    geco_2022_fossil_fuels_15c_raw,
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

  out <- bridge_technologies(out, technology_bridge)

  out <- tidyr::pivot_longer(
    out,
    cols = tidyr::matches("x20[0-9]{2}$"),
    names_to = "year",
    names_prefix = "x",
    names_transform = list(year = as.numeric),
    values_to = "value",
    values_ptypes = numeric()
  )

  dplyr::mutate(
    out,
    year = as.double(.data$year)
  )
}
