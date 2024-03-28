#' Prepare GECO 2023 scenario data
#'
#' @param geco_2023_aviation_15c_raw A raw GECO 2023 automotive 1.5 scenario data-frame.
#' @param geco_2023_aviation_ndc_raw A raw GECO 2023 automotive NDC scenario data-frame.
#' @param geco_2023_aviation_ref_raw A raw GECO 2023 automotive Reference scenario data-frame.
#' @param geco_2023_fossil_fuels_15c_raw A raw GECO 2023 fossil fuels 1.5 scenario data-frame.
#' @param geco_2023_fossil_fuels_ndc_raw A raw GECO 2023 fossil fuels NDC scenario data-frame.
#' @param geco_2023_fossil_fuels_ref_raw A raw GECO 2023 fossil fuels Reference scenario data-frame.
#' @param geco_2023_power_cap_15c_raw A raw GECO 2023 power capacity 1.5 scenario data-frame.
#' @param geco_2023_power_cap_ndc_raw A raw GECO 2023 power capacity NDC scenario data-frame.
#' @param geco_2023_power_cap_ref_raw A raw GECO 2023 power capacity Reference scenario data-frame.
#' @param geco_2023_steel_15c_raw A raw GECO 2023 steel 1.5 scenario data-frame.
#' @param geco_2023_steel_ndc_raw A raw GECO 2023 steel NDC scenario data-frame.
#' @param geco_2023_steel_ref_raw A raw GECO 2023 steel Reference scenario data-frame.
#' @param geco_2023_supplement_15c_raw A raw GECO 2023 supplemental 1.5 scenario data-frame.
#' @param geco_2023_supplement_ndc_raw A raw GECO 2023 supplemental NDC scenario data-frame.
#' @param geco_2023_supplement_ref_raw A raw GECO 2023 supplemental Reference scenario data-frame.
#'
#' @return A prepared GECO 2023 scenario data-frame.
#'
#' @importFrom dplyr %>%
#'
#' @export

prepare_geco_2023_scenario <- function(geco_2023_aviation_15c_raw,
                                       geco_2023_aviation_ndc_raw,
                                       geco_2023_aviation_ref_raw,
                                       geco_2023_fossil_fuels_15c_raw,
                                       geco_2023_fossil_fuels_ndc_raw,
                                       geco_2023_fossil_fuels_ref_raw,
                                       geco_2023_power_cap_15c_raw,
                                       geco_2023_power_cap_ndc_raw,
                                       geco_2023_power_cap_ref_raw,
                                       geco_2023_steel_15c_raw,
                                       geco_2023_steel_ndc_raw,
                                       geco_2023_steel_ref_raw,
                                       geco_2023_supplement_15c_raw,
                                       geco_2023_supplement_ndc_raw,
                                       geco_2023_supplement_ref_raw) {
  stopifnot(
    is.data.frame(geco_2023_aviation_15c_raw),
    is.data.frame(geco_2023_aviation_ndc_raw),
    is.data.frame(geco_2023_aviation_ref_raw),
    is.data.frame(geco_2023_fossil_fuels_15c_raw),
    is.data.frame(geco_2023_fossil_fuels_ndc_raw),
    is.data.frame(geco_2023_fossil_fuels_ref_raw),
    is.data.frame(geco_2023_power_cap_15c_raw),
    is.data.frame(geco_2023_power_cap_ndc_raw),
    is.data.frame(geco_2023_power_cap_ref_raw),
    is.data.frame(geco_2023_steel_15c_raw),
    is.data.frame(geco_2023_steel_ndc_raw),
    is.data.frame(geco_2023_steel_ref_raw),
    is.data.frame(geco_2023_supplement_15c_raw),
    is.data.frame(geco_2023_supplement_ndc_raw),
    is.data.frame(geco_2023_supplement_ref_raw)
  )

  geco_2023_supplement_15c <-
    extract_power_emissions(geco_2023_supplement_15c_raw, "1.5C")

  geco_2023_supplement_ndc <-
    extract_power_emissions(geco_2023_supplement_ndc_raw, "NDC-LTS")

  geco_2023_supplement_ref <-
    extract_power_emissions(geco_2023_supplement_ref_raw, "Reference")

  power_emissions_intensity <-
    dplyr::bind_rows(
      "1.5C" = geco_2023_supplement_15c,
      "NDC-LTS" = geco_2023_supplement_ndc,
      "Reference" = geco_2023_supplement_ref,
      .id = "Scenario"
    )

  geco_2023_aviation <-
    prepare_geco_2023_aviation_scenario(
      power_emissions_intensity,
      geco_2023_aviation_15c_raw,
      geco_2023_aviation_ndc_raw,
      geco_2023_aviation_ref_raw
    )

  geco_2023_fossil_fuels <-
    prepare_geco_2023_fossil_fuels_scenario(
      geco_2023_fossil_fuels_15c_raw,
      geco_2023_fossil_fuels_ndc_raw,
      geco_2023_fossil_fuels_ref_raw
    )

  geco_2023_power_cap <-
    prepare_geco_2023_power_cap_scenario(
      geco_2023_power_cap_15c_raw,
      geco_2023_power_cap_ndc_raw,
      geco_2023_power_cap_ref_raw
    )

  geco_2023_steel <-
    prepare_geco_2023_steel_scenario(
      power_emissions_intensity,
      geco_2023_steel_15c_raw,
      geco_2023_steel_ndc_raw,
      geco_2023_steel_ref_raw
    )

  out <-
    rbind(
      geco_2023_aviation,
      geco_2023_fossil_fuels,
      geco_2023_power_cap,
      geco_2023_steel
    )

  out <- bridge_geographies(out, geco_2023_geography_bridge)

  if (any(is.na(unique(out$scenario)))) {
    stop("Unique scenario names are not well-defined. Please review!")
  }

  pacta.data.validation::validate_intermediate_scenario_output(out)
  out
}


extract_power_emissions <- function(data, scenario) {
  data %>%
    dplyr::select(-"1990") %>%
    dplyr::filter(.data[["World"]] %in%  c(
      "Power generation/District heating",
      "Gross Elec. Generation (TWhe)"
    )) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      names_transform = as.integer
    ) %>%
    tidyr::pivot_wider(names_from = "World") %>%
    dplyr::rename(
      absolute_emissions = "Power generation/District heating",
      generation = "Gross Elec. Generation (TWhe)"
    ) %>%
    dplyr::mutate(value = .data[["absolute_emissions"]] / .data[["generation"]]) %>%
    interpolate_yearly()  %>%
    dplyr::mutate(Scenario = .env[["scenario"]]) %>%
    dplyr::select("year", power_emission_intensity = "value", "Scenario")
}


prepare_geco_2023_aviation_scenario <- function(power_emissions_intensity,
                                                geco_2023_aviation_15c_raw,
                                                geco_2023_aviation_ndc_raw,
                                                geco_2023_aviation_ref_raw) {
  out <-
    dplyr::bind_rows(
      geco_2023_aviation_15c_raw,
      geco_2023_aviation_ndc_raw,
      geco_2023_aviation_ref_raw
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value",
      values_ptypes = numeric()
    ) %>%
    dplyr::filter(.data[["Passenger/Freight"]] == "Passenger") %>%
    tidyr::unite("Variable", "Variable", "Technology", "Unit", sep = " ") %>%
    tidyr::pivot_wider(names_from = "Variable") %>%
    dplyr::filter(!.data[["year"]] %in% c(2060, 2070)) %>% # we don't have power EI after 2050
    dplyr::rename(
      activity_pkm = "Activity PKM pkm",
      electricity_consumption_ktoe = "Final energy consumption Electricity ktoe",
      oil_consumption_ktoe = "Final energy consumption Oil ktoe"
    ) %>%
    dplyr::left_join(power_emissions_intensity, by = c("Scenario", "year")) %>%
    # 73.3 conversion from MJ in g of CO2 - figure from a range of data sources:
    # https://theicct.org/sites/default/files/publications/Alt-aviation-fuel-sustainability-mar2021.pdf
    # 41870 conversion of toe in MJ:
    # https://www.iea.org/data-and-statistics/data-tools/unit-converter
    dplyr::mutate(
      value = .data[["oil_consumption_ktoe"]] * 73.3 * 41.870 / .data[["activity_pkm"]] / 1000000
    ) %>%
    dplyr::mutate(
      units = "tCO2/pkm",
      indicator = "Emission Intensity",
      technology = NA_character_,
      sector = "Aviation"
    ) %>%
    dplyr::select(
      source = "GECO",
      scenario = "Scenario",
      scenario_geography = "Region",
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


prepare_geco_2023_fossil_fuels_scenario <- function(geco_2023_fossil_fuels_15c_raw,
                                                    geco_2023_fossil_fuels_ndc_raw,
                                                    geco_2023_fossil_fuels_ref_raw) {
  out <-
    dplyr::bind_rows(
      geco_2023_fossil_fuels_15c_raw,
      geco_2023_fossil_fuels_ndc_raw,
      geco_2023_fossil_fuels_ref_raw
    ) %>%
    dplyr::mutate(sector = ifelse(.data[["Fuel"]] == "Coal", "Coal", "Oil&Gas")) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value",
      values_ptypes = numeric()
    ) %>%
    dplyr::select(
      source = "GECO",
      scenario = "Scenario",
      scenario_geography = "Region",
      "sector",
      technology = "Fuel",
      indicator = "Variable",
      units = "Unit",
      "year",
      "value"
    )

  pacta.data.validation::validate_intermediate_scenario_output(out)
  out
}


prepare_geco_2023_power_cap_scenario <- function(geco_2023_power_cap_15c_raw,
                                                 geco_2023_power_cap_ndc_raw,
                                                 geco_2023_power_cap_ref_raw) {
  out <-
    dplyr::bind_rows(
      geco_2023_power_cap_15c_raw,
      geco_2023_power_cap_ndc_raw,
      geco_2023_power_cap_ref_raw
    ) %>%
    # actually those technology are already included in Coal/Gas/Biomass and
    # capacities are actually double counted if we don't filter
    dplyr::filter(!.data[["Technology"]] %in% c("Coal with CCUS", "Gas with CCUS", "Biomass & Waste CCUS")) %>%
    dplyr::mutate(Technology = dplyr::case_when(
      .data[["Technology"]] == "Coal" ~ "CoalCap",
      .data[["Technology"]] == "Oil" ~ "OilCap",
      .data[["Technology"]] == "Gas" ~ "GasCap",
      .data[["Technology"]] == "Other" ~ "RenewablesCap",
      TRUE ~ .data[["Technology"]]
    )) %>%
    dplyr::left_join(
      geco_2023_technology_bridge,
      by = c("Technology" = "scenario_technology_name")
    ) %>%
    dplyr::select(-"Technology") %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value",
      values_ptypes = numeric()
    ) %>%
    # summarise renewables
    dplyr::summarise(
      value = sum(.data[["value"]], na.rm = TRUE),
      .by = -"value"
    ) %>%
    # raw data is off by a magnitude of 1000. Provided capacity values are MW,
    # but unit displays GW. We fix by dividing by 1000 and thus keep our
    # standardized unit of GW power capacity
    dplyr::mutate(value = .data[["value"]] / 1000) %>%
    dplyr::mutate(sector = "Power") %>%
    dplyr::select(
      source = "GECO",
      scenario = "Scenario",
      scenario_geography = "Region",
      "sector",
      technology = "standardized_technology_name",
      indicator = "Variable",
      units = "Unit",
      "year",
      "value"
    )

  pacta.data.validation::validate_intermediate_scenario_output(out)
  out
}


prepare_geco_2023_steel_scenario <- function(power_emissions_intensity,
                                             geco_2023_steel_15c_raw,
                                             geco_2023_steel_ndc_raw,
                                             geco_2023_steel_ref_raw) {
  out <-
    dplyr::bind_rows(
      geco_2023_steel_15c_raw,
      geco_2023_steel_ndc_raw,
      geco_2023_steel_ref_raw
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value",
      values_ptypes = numeric()
    ) %>%
    tidyr::unite("Variable", "Variable", "Fuel", "Unit", sep = " ") %>%
    tidyr::pivot_wider(names_from = "Variable") %>%
    dplyr::left_join(power_emissions_intensity, by = c("Scenario", "year")) %>%
    dplyr::rename(
      total_production = "Production Total kt",
      consumption_electricity = "Final Energy Consumption Electricity mtoe",
      total_emissions = "CO2 Emissions Total mtCO2"
    ) %>%
    dplyr::mutate(
      emissions_intensity_scope1 = .data[["total_emissions"]] / .data[["total_production"]] * 1000,
      emissions_intensity_scope2 = .data[["consumption_electricity"]] * 11.63 * .data[["power_emission_intensity"]]  / .data[["total_production"]] * 1000,
      emissions_intensity = .data[["emissions_intensity_scope1"]] + .data[["emissions_intensity_scope2"]]
    ) %>%
    dplyr::mutate(
      units = "tCO2/t Steel",
      Technology = NA_character_,
      sector = "Steel",
      indicator = "Emission Intensity"
    ) %>%
    dplyr::select(
      source = "GECO",
      scenario = "Scenario",
      scenario_geography = "Region",
      "sector",
      technology = "Technology",
      "indicator",
      "units",
      "year",
      value = "emissions_intensity"
    )

  pacta.data.validation::validate_intermediate_scenario_output(out)
  out
}
