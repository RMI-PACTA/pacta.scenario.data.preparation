#' Prepare WEO 2023 scenario data
#'
#' @param weo_2023_ext_data_regions_raw A data frame containing a raw
#'   `WEO2023_Extended_Data_Regions.csv` import.
#' @param weo_2023_ext_data_world_raw A data frame containing a raw
#'   `WEO2023_Extended_Data_World.csv` import.
#' @param weo_2023_fig_chptr_3_raw A tidyxl data frame (with a `formats`
#'   attribute) containing a raw import of `WEO2023_Figures_Chapter_03.xlsx`.
#' @param iea_global_ev_raw A data frame containing a raw `IEA Global EV Data
#'   2023.csv` import.
#' @param mpp_ats_raw A tidyxl data frame (with a `formats` attribute)
#'   containing a raw import of `2022-08-12 - MPP ATS - RPK and GHG
#'   intensity.xlsx`.
#'
#' @return A prepared WEO 2023 scenario data-frame.
#'
#' @importFrom dplyr %>%
#'
#' @export

prepare_weo_2023_scenario <- function(weo_2023_ext_data_regions_raw,
                                      weo_2023_ext_data_world_raw,
                                      weo_2023_fig_chptr_3_raw,
                                      iea_global_ev_raw,
                                      mpp_ats_raw) {
  # weo_2023_automotive <- weo_2023_extract_automotive(weo_2023_fig_chptr_3_raw)
  weo_2023_aviation <- weo_2023_extract_aviation(mpp_ats_raw, weo_2023_ext_data_world_raw)
  weo_2023_fossil_fuels <- weo_2023_extract_fossil_fuels(weo_2023_fig_chptr_3_raw)
  # weo_2023_power <- weo_2023_extract_power()
  weo_2023_steel_cement <- weo_2023_extract_steel_and_cement(weo_2023_ext_data_world_raw, weo_2023_fig_chptr_3_raw)

  out <-
    dplyr::bind_rows(
      # weo_2023_automotive,
      weo_2023_aviation,
      weo_2023_fossil_fuels,
      # weo_2023_power,
      weo_2023_steel_cement
    ) %>%
    dplyr::relocate(
      "source",
      "scenario",
      "scenario_geography",
      "sector",
      "indicator",
      "units",
      "year",
      "technology",
      "value"
    )

  validate_intermediate_scenario_output(out)

  out

  #   dplyr::mutate(
  #     scenario = dplyr::case_when(
  #       scenario == "Announced Pledges Scenario" ~ "APS",
  #       scenario == "Sustainable Development Scenario" ~ "SDS",
  #       scenario == "Stated Policies Scenario" ~ "STEPS",
  #       scenario == "Net Zero Emissions by 2050 Scenario" ~ "NZE",
  #       TRUE ~ scenario,
  #     )
  #   ) %>%
  #   dplyr::mutate(
  #     scenario = dplyr::if_else(scenario == "NZE", "NZE_2050", scenario)
  #   ) %>%
  #   dplyr::mutate(
  #     source = dplyr::case_when(
  #       source == "World Energy Outlook 2023" ~ "WEO2023",
  #       TRUE ~ "WEO2023"
  #     )
  #   ) %>%
  #   dplyr::summarize(value = sum(value), .by = -value)
  #
  #   bridge_geographies(weo_2023_geography_bridge) %>%
  #   bridge_technologies(weo_2023_technology_bridge)
  #   dplyr::mutate(
  #     scenario_geography = dplyr::if_else(
  #       scenario_geography == "World",
  #       "Global",
  #       scenario_geography
  #     )
  #   ) %>%
  #   dplyr::left_join(
  #     technology_bridge,
  #     by = c(technology = "TechnologyAll")
  #   ) %>%
  #   dplyr::select(-"technology") %>%
  #   dplyr::rename(technology = "TechnologyName") %>%
  #   dplyr::summarize(value = sum(value), .by = -value)
  #
  # out %>%
  #   bridge_technologies(weo_2023_technology_bridge) %>%
  #   dplyr::summarize(
  #     value = sum(.data[["value"]], na.rm = TRUE),
  #     .by = c(
  #       "source",
  #       "scenario",
  #       "scenario_geography",
  #       "sector",
  #       "technology",
  #       "indicator",
  #       "units",
  #       "year"
  #     )
  #   ) %>%
  #   bridge_geographies(weo_2023_geography_bridge) %>%
  #   dplyr::arrange(
  #     .data[["scenario_geography"]],
  #     .data[["sector"]],
  #     .data[["technology"]],
  #     .data[["year"]]
  #   ) %>%
  #   dplyr::relocate(
  #     "source",
  #     "scenario",
  #     "scenario_geography",
  #     "sector",
  #     "technology",
  #     "indicator",
  #     "units",
  #     "year",
  #     "value"
  #   )
}


weo_2023_extract_power <- function() {


  validate_intermediate_scenario_output(weo_2023_power)

  weo_2023_power
}


weo_2023_extract_automotive <- function(weo_2023_fig_chptr_3_raw) {
  weo_2023_auto_tech_share <- weo_2023_extract_auto_tech_share(weo_2023_fig_chptr_3_raw)



  validate_intermediate_scenario_output(weo_2023_automotive)

  weo_2023_automotive
}


weo_2023_extract_fossil_fuels <- function(weo_2023_fig_chptr_3_raw) {
  weo_2023_oil <- weo_2023_extract_oil(weo_2023_fig_chptr_3_raw)
  weo_2023_gas <- weo_2023_extract_gas(weo_2023_fig_chptr_3_raw)
  weo_2023_coal <- weo_2023_extract_coal(weo_2023_fig_chptr_3_raw)

  weo_2023_fossil_fuels <-
    dplyr::bind_rows(weo_2023_coal, weo_2023_gas, weo_2023_oil)  %>%
    dplyr::mutate(
      source = "WEO2023",
      scenario_geography = "Global",
      indicator = "Supply"
    ) %>%
    dplyr::select(
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

  validate_intermediate_scenario_output(weo_2023_fossil_fuels)

  weo_2023_fossil_fuels
}


weo_2023_extract_steel_and_cement <- function(weo_2023_ext_data_world_raw,
                                 weo_2023_fig_chptr_3_raw) {
  weo_2023_steel_cement_electricity_demand_raw <-
    weo_2023_fig_chptr_3_raw %>%
    dplyr::filter(sheet == "3.6") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 42, 51)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 11)) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename(
      "sector" = 1L,
      "2022" = 2L,
      "empty_1" = 3L,
      "STEPS_2030" = 4L,
      "APS_2030" = 5L,
      "NZE_2030" = 6L,
      "empty_2" = 7L,
      "STEPS_2050" = 8L,
      "APS_2050" = 9L,
      "NZE_2050" = 10L
    )

  weo_2023_extended_data_world <-
    weo_2023_ext_data_world_raw %>%
    dplyr::rename(
      publication = "PUBLICATION",
      scenario = "SCENARIO",
      category = "CATEGORY",
      flow = "FLOW",
      unit = "UNIT",
      region = "REGION",
      year = "YEAR",
      value = "VALUE"
    )

  weo_2023_extended_data_steel_cement_nze <-
    weo_2023_extended_data_world %>%
    dplyr::filter(
      scenario == "Net Zero Emissions by 2050 Scenario",
      flow %in%  c("Iron and steel", "Non-metallic minerals: cement")
    )

  weo_2023_steel_cement_production <-
    weo_2023_extended_data_steel_cement_nze %>%
    dplyr::filter(category == "Industrial material production") %>%
    dplyr::mutate(
      sector = dplyr::if_else(
        flow == "Iron and steel",
        "Steel",
        "Cement"
      )
    ) %>%
    dplyr::mutate(scenario = "NZE") %>%
    dplyr::rename(production = "value")

  weo_2023_steel_cement_emissions_scope1 <-
    weo_2023_extended_data_steel_cement_nze %>%
    dplyr::filter(category == "CO2 combustion and process") %>%
    dplyr::mutate(
      sector = dplyr::if_else(
        flow == "Iron and steel",
        "Steel",
        "Cement")
    ) %>%
    dplyr::mutate(scenario = "NZE") %>%
    dplyr::rename(absolute_emission_scope1 = "value")

  weo_2023_steel_cement_electricity_demand <-
    weo_2023_steel_cement_electricity_demand_raw %>%
    dplyr::select(-c("empty_1", "empty_2")) %>%
    dplyr::mutate(
      STEPS_2022 = .data[["2022"]],
      NZE_2022 = .data[["2022"]],
      APS_2022 = .data[["2022"]],
      `2022` = NULL
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        "NZE_2022",
        "APS_2022",
        "STEPS_2022",
        "NZE_2030",
        "APS_2030",
        "STEPS_2030",
        "NZE_2050",
        "APS_2050",
        "STEPS_2050"
      ),
      values_to = "electricity_demand"
    ) %>%
    tidyr::separate(
      col = "name",
      into = c("scenario", "year"),
      sep = "_"
    ) %>%
    dplyr::mutate(
      year = as.integer(year),
      electricity_demand = as.double(electricity_demand)) %>%
    dplyr::filter(
      sector %in% c("Iron and steel", "Cement"),
      scenario == "NZE") %>%
    dplyr::mutate(
      sector = ifelse(sector == "Iron and steel", "Steel", "Cement")
    )

  weo_2023_electricity_generation_nze <-
    weo_2023_extended_data_world %>%
    dplyr::filter(
      scenario == "Net Zero Emissions by 2050 Scenario",
      flow %in%  c("Electricity generation"),
      category == "CO2 total intensity"
    ) %>%
    dplyr::mutate(scenario = "NZE") %>%
    dplyr::rename(emission_intensity_power = "value")


  weo_2023_steel_cement_emission_scope2 <-
    weo_2023_steel_cement_electricity_demand %>%
    dplyr::left_join(
      weo_2023_electricity_generation_nze,
      by = c("scenario", "year")
    ) %>%
    dplyr::mutate(
      absolute_emission_scope2 =
        .data[["electricity_demand"]] * .data[["emission_intensity_power"]] / 1000,
      unit = "Mt CO2"
    ) %>%
    dplyr::select("sector", "scenario", "year", "unit", "absolute_emission_scope2")

  weo_2023_steel_cement <-
    weo_2023_steel_cement_emissions_scope1 %>%
    dplyr::left_join(
      weo_2023_steel_cement_emission_scope2,
      by = c("scenario", "year", "sector", "unit")
    ) %>%
    dplyr::left_join(
      weo_2023_steel_cement_production,
      by = c("scenario", "year", "publication", "sector", "flow", "region"),
      suffix = c("_emission_scope2", "_emission_scope1")
    ) %>%
    dplyr::mutate(
      value = (absolute_emission_scope1 + absolute_emission_scope2) / production,
      source = "WEO2023"
    ) %>%
    dplyr::distinct(source, scenario, sector, year, value) %>%
    dplyr::mutate(
      scenario_geography = "Global",
      units = dplyr::if_else(
        sector == "Steel",
        "tCO2/t Steel",
        "tCO2/t Cement"
      ),
      indicator = "Emission Intensity",
      technology = NA_character_
    ) %>%
    dplyr::filter(!is.na(.data[["value"]]))

  validate_intermediate_scenario_output(weo_2023_steel_cement)

  weo_2023_steel_cement
}


weo_2023_extract_oil <- function(weo_2023_fig_chptr_3_raw) {
  weo_2023_fig_chptr_3_raw %>%
    dplyr::filter(sheet == "Table.3.5") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 13, 32)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 12)) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename(
      "technology" = 1L,
      "historic_2010" = 2L,
      "STEPS_2022" = 3L,
      "STEPS_2030" = 4L,
      "STEPS_2050" = 5L,
      "empty_1" = 6L, # there are some empty columns in the input data
      "APS_2030" = 7L,
      "APS_2050" = 8L,
      "empty_2" = 9L, # there are some empty columns in the input data
      "NZE_2030" = 10L,
      "NZE_2050" = 11L
    ) %>%
    dplyr::select(-c("empty_1", "empty_2", "historic_2010")) %>%
    dplyr::filter(
      technology %in% c("World oil supply", "Natural gas liquids")
    ) %>%
    dplyr::mutate(
      # assumption: all scenarios share same baseline
      APS_2022 = STEPS_2022,
      NZE_2022 = STEPS_2022
    ) %>%
    tidyr::pivot_longer(
      cols = -"technology",
      names_to = c("scenario", "year"),
      names_sep = "_",
      names_transform = list(scenario = as.character, year = as.integer)
    ) %>%
    tidyr::pivot_wider(
      names_from = "technology",
      values_from = "value"
    ) %>%
    dplyr::rename(
      natural_gas_liquids = "Natural gas liquids",
      oil = "World oil supply"
    ) %>%
    dplyr::mutate(value = .data[["oil"]] - .data[["natural_gas_liquids"]]) %>%
    dplyr::mutate(
      sector = "Oil&Gas",
      technology = "Oil",
      units = "mb/d"
    )
}


weo_2023_extract_gas <- function(weo_2023_fig_chptr_3_raw) {
  weo_2023_fig_chptr_3_raw %>%
    dplyr::filter(sheet == "Table.3.6") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 11, 33)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 12)) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename(
      "sector" = 1L,
      "historic_2010" = 2L,
      "STEPS_2022" = 3L,
      "STEPS_2030" = 4L,
      "STEPS_2050" = 5L,
      "empty_1" = 6L, # there are some empty columns in the input data
      "APS_2030" = 7L,
      "APS_2050" = 8L,
      "empty_2" = 9L, # there are some empty columns in the input data
      "NZE_2030" = 10L,
      "NZE_2050" = 11L
    ) %>%
    dplyr::select(-c("empty_1", "empty_2", "historic_2010")) %>%
    dplyr::filter(sector == "Natural gas production (bcm)") %>%
    dplyr::mutate(
      APS_2022 = STEPS_2022,
      NZE_2022 = STEPS_2022
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        "STEPS_2022",
        "STEPS_2030",
        "STEPS_2050",
        "APS_2030",
        "APS_2050",
        "NZE_2030",
        "NZE_2050",
        "APS_2022",
        "NZE_2022"
      )
    ) %>%
    tidyr::separate_wider_delim(
      col = "name",
      names = c("scenario", "year"),
      delim = "_"
    ) %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::mutate(
      sector = "Oil&Gas",
      technology = "Gas",
      units = "bcm"
    )
}


weo_2023_extract_coal <- function(weo_2023_fig_chptr_3_raw) {
  weo_2023_fig_chptr_3_raw %>%
    dplyr::filter(sheet == "3.28") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 41, 44)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 20)) %>%
    dplyr::mutate(content =
      dplyr::if_else(.data[["content"]] == "#N/A", NA, .data[["content"]])
    ) %>%
    dplyr::mutate(data_type =
      dplyr::if_else(.data[["data_type"]] == "error", "numeric", .data[["data_type"]])
    ) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename(
      "scenario" = 1L,
      "2010" = 2L,
      "2011" = 3L,
      "2012" = 4L,
      "2013" = 5L,
      "2014" = 6L,
      "2015" = 7L,
      "2016" = 8L,
      "2017" = 9L,
      "2018" = 10L,
      "2019" = 11L,
      "2020" = 12L,
      "2021" = 13L,
      "2022" = 14L,
      "2030" = 15L,
      "2035" = 16L,
      "2040" = 17L,
      "2045" = 18L,
      "2050" = 19L
    ) %>%
    dplyr::filter(scenario != "Historical") %>%
    dplyr::select("scenario", "2022","2030", "2035", "2040", "2045", "2050") %>%
    tidyr::pivot_longer(
      cols = c("2022","2030", "2035", "2040", "2045", "2050"),
      names_to = "year",
      names_transform = as.integer,
    ) %>%
    dplyr::mutate(
      sector = "Coal",
      technology = "Coal",
      units = "Mtce"
    )
}


weo_2023_extract_aviation <- function(mpp_ats_raw, weo_2023_ext_data_world_raw) {
  mpp_commercial_passenger_aviation_raw <-
    mpp_ats_raw %>%
    dplyr::filter(dplyr::between(.data[["row"]], 8, 20)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 34)) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename("variable" = 1L) %>%
    dplyr::rename_with(~ as.character(2019:2050), 2:33)

  mpp_commercial_cargo_aviation_raw <-
    mpp_ats_raw %>%
    dplyr::filter(dplyr::between(.data[["row"]], 26, 38)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 34)) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename("variable" = 1L) %>%
    dplyr::rename_with(~ as.character(2019:2050), 2:33)

  weo_2023_extended_world <-
    weo_2023_ext_data_world_raw %>%
    dplyr::rename(
      source = "PUBLICATION",
      scenario = "SCENARIO",
      variable = "CATEGORY",
      technology = "PRODUCT",
      flow = "FLOW",
      unit = "UNIT",
      region = "REGION",
      year = "YEAR",
      value = "VALUE"
    )

  mpp_mutual_preparation <- function(data) {
    data %>%
      dplyr::filter(
        variable == "Annual GHG Emissions in tCO2e - PRU"
      ) %>%
      tidyr::pivot_longer(
        cols = tidyr::matches("20[0-9]{2}$"),
        names_to = "year",
        names_transform = as.integer,
        values_to = "value"
      ) %>%
      dplyr::filter(value != 0)
  }

  mpp_commercial_passenger_aviation <-
    mpp_commercial_passenger_aviation_raw %>%
    mpp_mutual_preparation()

  mpp_commercial_cargo_aviation <-
    mpp_commercial_cargo_aviation_raw %>%
    mpp_mutual_preparation()

  mpp_commercial_aviation <-
    mpp_commercial_passenger_aviation %>%
    dplyr::left_join(
      mpp_commercial_cargo_aviation,
      by = c("variable", "year"),
      suffix = c("_passenger", "_cargo")
    ) %>%
    dplyr::mutate(
      percent_passenger_of_total =
        .data[["value_passenger"]] / (.data[["value_passenger"]] + .data[["value_cargo"]])
    )

  weo_2023_aviation_emissions <-
    weo_2023_ext_data_world_raw %>%
    dplyr::rename_with(.fn = tolower) %>%
    dplyr::filter(
      flow == "Total aviation (domestic and bunkers)",
      scenario == "Net Zero Emissions by 2050 Scenario",
      category == "CO2 combustion"
    ) %>%
    dplyr::rename(emissions = "value")

  weo_2023_aviation_pkm <-
    weo_2023_ext_data_world_raw %>%
    dplyr::rename_with(.fn = tolower) %>%
    dplyr::filter(
      flow == "Total aviation (domestic and bunkers)",
      scenario == "Net Zero Emissions by 2050 Scenario",
      category == "Activity of stock"
    ) %>%
    dplyr::rename(passenger_km = "value")

  # combine aviation data
  weo_2023_aviation <-
    weo_2023_aviation_emissions %>%
    dplyr::left_join(
      weo_2023_aviation_pkm,
      by = c("scenario", "year", "publication", "product", "region", "flow"),
      suffix = c("_emissions", "_pkm")
    ) %>%
    dplyr::left_join(
      mpp_commercial_aviation,
      by = "year",
      suffix = c("_weo", "_mpp")
    ) %>%
    dplyr::filter(!is.na(.data[["percent_passenger_of_total"]])) %>%
    dplyr::mutate(
      # convert to tCO2e per passenger km from Mt CO2e per passenger km
      value =
        (.data[["percent_passenger_of_total"]]) *
        (.data[["emissions"]] * 1e6) / # convert Mt CO2 to t CO2
        (.data[["passenger_km"]] * 1e9) # convert billions passenger km to passenger km
    ) %>%
    dplyr::select("value", "year") %>%
    dplyr::mutate(
      scenario = "NZE",
      source = "WEO2023",
      scenario_geography = "Global",
      units = "tCO2/pkm",
      indicator = "Emission Intensity",
      sector = "Aviation",
      technology = "Passenger"
    )

  validate_intermediate_scenario_output(weo_2023_aviation)

  weo_2023_aviation
}


weo_2023_extract_auto_tech_share <- function(weo_2023_fig_chptr_3_raw) {
  weo_2023_fig_chptr_3_raw %>%
    dplyr::filter(sheet == "3.34") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 39, 42)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 2, 8)) %>%
    unpivotr::rectify() %>%
    dplyr::select(-"row/col") %>%
    dplyr::rename(
      "technology" = 1L,
      "empty_1" = 2L,
      "STEPS_2022" = 3L,
      "empty_2" = 4L,
      "STEPS_2030" = 5L,
      "APS_2030" = 6L,
      "NZE_2030" = 7L
    ) %>%
    dplyr::select(-c("empty_1", "empty_2")) %>%
    dplyr::filter(!is.na(technology)) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        technology == "Battery electric" ~ "BEV",
        technology == "Fuel cell electric" ~ "FuelCell",
        technology == "Plug-in hybrid electric" ~ "PHEV",
        TRUE ~ technology
      )
    ) %>%
    dplyr::mutate(
      # assumption: shared baseline
      APS_2022 = STEPS_2022,
      NZE_2022 = STEPS_2022
    )  %>%
    tidyr::pivot_longer(
      cols = c(
        "STEPS_2022",
        "STEPS_2030",
        "APS_2022",
        "APS_2030",
        "NZE_2022",
        "NZE_2030"
      ),
      names_to = "name",
      values_to = "value_share"
    ) %>%
    tidyr::separate_wider_delim(
      col = "name",
      names = c("scenario", "year"),
      delim = "_"
    ) %>%
    dplyr::mutate(
      year = as.integer(year),
      region = "World",
      units = "Vehicles"
    )
}
