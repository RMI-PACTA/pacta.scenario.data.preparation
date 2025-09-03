#' Prepare WEO 2024 scenario data
#'
#' @param weo_2024_ext_data_regions_raw A data frame containing a raw
#'   `WEO2024_Extended_Data_Regions.csv` import.
#' @param weo_2024_ext_data_world_raw A data frame containing a raw
#'   `WEO2024_Extended_Data_World.csv` import.
#' @param weo_2024_fig_chptr_3_raw A tidyxl data frame containing a raw import
#'   of `WEO2024_Figures_Chapter_03.xlsx`.
#' @param iea_global_ev_2024_raw A data frame containing a raw import of 'electric-vehicle-sales-by-region-and-scenario-2030-and-2035.xlsx'
#' sheet : electric-vehicle-sales-by-regio
#' @param iea_sales_share_ev A data frame containing a raw import of 'electric-vehicle-sales-by-region-and-scenario-2030-and-2035.xlsx'
#' - sheet: electric vehicle share-ev
#' @param mpp_ats_raw A tidyxl data frame containing a raw import of `2022-08-12
#'   - MPP ATS - RPK and GHG intensity.xlsx`.
#' @return A prepared WEO 2024 scenario data-frame.
#'
#' @importFrom dplyr %>%
#'
#' @export
prepare_weo_2024_hybrid_in_ev_scenario <- function(weo_2024_ext_data_regions_raw,
                                      weo_2024_ext_data_world_raw,
                                      weo_2024_fig_chptr_3_raw,
                                      iea_global_ev_2024_raw,
                                      iea_sales_share_ev,
                                      mpp_ats_raw) {
  weo_2024_hybrid_in_ev_automotive <- weo_2024_hybrid_in_ev_extract_automotive(
    iea_global_ev_2024_raw,
    iea_sales_share_ev
  )
  weo_2024_aviation <- weo_2024_extract_aviation(mpp_ats_raw, weo_2024_ext_data_world_raw)
  weo_2024_fossil_fuels <- weo_2024_extract_fossil_fuels(weo_2024_fig_chptr_3_raw)
  weo_2024_power <- weo_2024_extract_power(weo_2024_ext_data_regions_raw, weo_2024_ext_data_world_raw)

  out <-
    dplyr::bind_rows(
      weo_2024_hybrid_in_ev_automotive,
      weo_2024_aviation,
      weo_2024_fossil_fuels,
      weo_2024_power
    ) %>%
    dplyr::mutate(
      source =
        dplyr::if_else(source == "World Energy Outlook 2024", "WEO2024", source)
    ) %>%
    dplyr::mutate(
      scenario = dplyr::case_when(
        .data[["scenario"]] == "Announced Pledges Scenario" ~ "APS",
        .data[["scenario"]] == "Sustainable Development Scenario" ~ "SDS",
        .data[["scenario"]] == "Stated Policies Scenario" ~ "STEPS",
        .data[["scenario"]] == "Net Zero Emissions by 2050 Scenario" ~ "NZE_2050",
        .data[["scenario"]] == "NZE" ~ "NZE_2050",
        TRUE ~ .data[["scenario"]],
      )
    ) %>%
    bridge_technologies(weo_2024_technology_bridge) %>%
    bridge_geographies(weo_2024_geography_bridge) %>%
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
    ) %>%
    dplyr::summarize(value = sum(.data[["value"]]), .by = -c("value"))

  pacta.data.validation::validate_intermediate_scenario_output(out)

  out
}


weo_2024_extract_power <- function(weo_2024_ext_data_regions_raw,
                                   weo_2024_ext_data_world_raw) {
  techs_out_of_pacta_scope <- c(
    # the following technologies are removed either because:
    # * they are out of PACTA scope
    # * to avoid double counting
    "Total", # avoid double counting
    "Battery storage",
    "Modern biomass",
    "Traditional use of biomass",
    "Total liquids",
    "Electricity",
    "Hydrogen and H2-based fuels",
    "Hydrogen-based fuels: liquid",
    "Total gases",
    "Hydrogen-based fuels: gaseous",
    "Total solid fuels",
    "District heat",
    "Modern bioenergy: liquid",
    "Ammonia" ,
    "Synthetic oil products",
    "Biomethane",
    "Hydrogen",
    "Synthetic methane",
    "Solid bioenergy incl. TUOB",
    "Other",
    "Fossil fuels: with CCUS",
    "Fossil fuels: unabated",
    "Bioenergy: with CCUS"
  )

  weo_2024_extended_regions <-
    weo_2024_ext_data_regions_raw %>%
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
    ) %>%
    dplyr::filter(
      .data[["unit"]] == "GW",
      !(.data[["technology"]] %in% techs_out_of_pacta_scope)
    )

  weo_2024_extended_world <-
    weo_2024_ext_data_world_raw %>%
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
    ) %>%
    dplyr::filter(
      .data[["unit"]] == "GW",
      !(.data[["technology"]] %in% techs_out_of_pacta_scope)
    ) %>%
    dplyr::mutate(year = as.double(.data[["year"]]))

  weo_2024_power_regions_aps_baseline <-
    weo_2024_extended_regions %>%
    dplyr::filter(
      # assumption: prior to and inclusive start year, APS is consistent with STEPS
      .data[["year"]] <= substr(config_name, start = 1, stop = 4)
    ) %>%
    dplyr::filter(.data[["scenario"]] == "Stated Policies Scenario") %>%
    dplyr::mutate(scenario = "Announced Pledges Scenario")

  weo_2024_power_regions_nze_baseline <-
    weo_2024_extended_regions %>%
    dplyr::filter(
      # assumption: prior to and inclusive start year, NZE is consistent with STEPS
      .data[["year"]] <= substr(config_name, start = 1, stop = 4)
    ) %>%
    dplyr::filter(
      .data[["scenario"]] == "Stated Policies Scenario",
      .data[["region"]] == "Advanced economies"
    ) %>%
    dplyr::mutate(scenario = "Net Zero Emissions by 2050 Scenario")

  weo_2024_extended_regions <-
    weo_2024_extended_regions %>%
    dplyr::filter(
      !(.data[["year"]] < 2030 & .data[["scenario"]] == "Net Zero Emissions by 2050 Scenario")
    )

  weo_2024_power_regions <-
    dplyr::bind_rows(
      weo_2024_extended_regions,
      weo_2024_power_regions_aps_baseline,
      weo_2024_power_regions_nze_baseline
    )

  weo_2024_power_no_renewables <-
    dplyr::bind_rows(
      weo_2024_power_regions,
      weo_2024_extended_world
    ) %>%
    dplyr::left_join(weo_2024_technology_bridge, by = c(technology = "scenario_technology_name")) %>%
    dplyr::filter(
      # for regional pathways, we must calculate renewables capacity in a more involved way below
      .data[["standardized_technology_name"]] != "RenewablesCap"
    )

  # If we sum all sub technology, we would miss geothermal or solar cpv
  # we'll obtain renewables capacities by subtracting hydro from total renewables
  # (total renewables contains hydro)
  weo_2024_power_regions_renewables <-
    weo_2024_extended_regions %>%
    dplyr::bind_rows(
      weo_2024_extended_world
    ) %>%
    dplyr::filter(.data[["technology"]] %in% c("Hydro", "Renewables")) %>%
    tidyr::pivot_wider(
      names_from = "technology",
      values_from = "value"
    ) %>%
    dplyr::mutate(
      value = .data[["Renewables"]] - .data[["Hydro"]],
      Renewables = NULL,
      Hydro = NULL,
      technology = "RenewablesCap"
    ) %>%
    dplyr::mutate(
      scenario = dplyr::if_else(
        is.na(.data[["scenario"]]),
        "Stated Policies Scenario",
        .data[["scenario"]]
      )
    )


  weo_2024_power_regions_renewables_aps_baseline <-
    weo_2024_power_regions_renewables %>%
    # assumption: prior to 2030, APS is consistent with STEPS
    dplyr::filter(.data[["year"]] < 2030) %>%
    dplyr::mutate(scenario = "Announced Pledges Scenario")

  weo_2024_power_regions_renewables_nze_baseline <-
    weo_2024_power_regions_renewables %>%
    # assumption: prior to 2030, NZE is consistent with STEPS
    dplyr::filter(.data[["year"]] < 2030) %>%
    dplyr::filter(.data[["region"]] == "Advanced economies") %>% # only region that we have a granular pathway for NZE2050
    dplyr::mutate(scenario = "Net Zero Emissions by 2050 Scenario")

  weo_2024_power <-
    dplyr::bind_rows(
      weo_2024_power_no_renewables,
      weo_2024_power_regions_renewables,
      weo_2024_power_regions_renewables_aps_baseline,
      weo_2024_power_regions_renewables_nze_baseline
    ) %>%
    dplyr::filter(.data[["unit"]] == "GW", !(.data[["technology"]] %in% techs_out_of_pacta_scope)) %>%
    dplyr::rename(
      units = "unit",
      scenario_geography = "region",
      indicator = "variable"
    ) %>%
    dplyr::mutate(
      sector= "Power",
      technology = dplyr::if_else(
        .data[["technology"]] == "Oil",
        "OilCap",
        .data[["technology"]]
      )
    ) %>%
    dplyr::select(
      c(
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
    ) %>%
    dplyr::distinct() # some baseline figures may be duplicated
  weo_2024_power
}


weo_2024_hybrid_in_ev_extract_automotive <- function(
    iea_global_ev_2024_raw,
    iea_sales_share_ev
) {
  # Prepare Global roadmap with 2 technologies
  # Electric Vehicles includes Battery Elevtric Vehicles (Electric in Asset Impact) and PHEV (Hybrid in Asset Impact)
  # This pathway is global

  iea_sales_longer <- iea_global_ev_2024_raw %>%
    tidyr::pivot_longer(
      cols = c("China", "Europe", "United States", "Japan", "India", "Other", "Global"),
      names_to = "Region",
      values_to = "Electric"
    ) %>%
    dplyr::select(-c(
      "Source: IEA, Licence: CC BY 4,0This data is subject to the IEA's terms and conditions: https://www,iea,org/t_c/termsandconditions/Units: million vehicles"
    )
    )

  iea_sales_longer_global <- iea_sales_longer %>%
    dplyr::filter(
      .data[["Region"]] == "Global",
      .data[["Technology"]] == "Electric vehicle"
    )

  iea_sales_share_ev_ldv <- iea_sales_share_ev %>%
    dplyr::filter(.data[["Vehicle Type"]] == "Light-duty vehicle") %>%
    tidyr::pivot_longer(
      cols = c("Stated Policies Scenario", "Announced Pledges Scenario", "Net Zero Scenario"),
      names_to = "Scenario",
      values_to = "Sales share"
    ) %>%
    dplyr::select(-c(
      "Source: IEA. Licence: CC BY 4.0This data is subject to the IEA's terms and conditions: https://www.iea.org/t_c/termsandconditions/Units: %"
    )
    ) %>%
    dplyr::mutate(
      Scenario = dplyr::case_when(
        .data[["Scenario"]] == "Stated Policies Scenario" ~ "STEPS",
        .data[["Scenario"]] == "Announced Pledges Scenario" ~ "APS",
        .data[["Scenario"]] == "Net Zero Scenario" ~ "NZE",
        TRUE ~ .data[["Scenario"]]
      ),
    Source = "WEO2024")


  iea_ev <- iea_sales_share_ev_ldv %>%
    dplyr::left_join(iea_sales_longer_global, by = c("Year", "Scenario"))

  iea_ev_ice <- iea_ev %>%
    dplyr::mutate(
      ICE = .data[["Electric"]] * (100 / .data[["Sales share"]] - 1)
    )

  iea_auto_end_loop <- iea_ev_ice %>%
    dplyr::select(-c("Unit.x", "Source.x", "Source.y", "Sales share", "Technology", "Vehicle Type")) %>%
    tidyr::pivot_longer(
      cols = c("Electric", "ICE"),
      names_to = "Technology",
      values_to = "Value"
    ) %>%
    dplyr::rename(Units = "Unit.y") %>%
    dplyr::mutate(Units = "# (in million)")

  weo_2024_automotive <- iea_auto_end_loop %>%
    dplyr::mutate(
      sector = "Automotive",
      indicator = "Sales",
      Technology = dplyr::case_when(
        .data[["Technology"]] == "BEV" ~ "Electric",
        .data[["Technology"]] == "PHEV" ~ "Hybrid",
        TRUE ~ .data[["Technology"]]
      )) %>%
    dplyr::rename(
      source = "Scenario Source",
      scenario = "Scenario",
      scenario_geography = "Region",
      technology = "Technology",
      units = "Units",
      year = "Year",
      value = "Value"
    ) %>%
    dplyr::select(
      "source",
      "scenario",
      "scenario_geography",
      "technology",
      "units",
      "year",
      "value",
      "sector",
      "indicator"
    )

  pacta.data.validation::validate_intermediate_scenario_output(weo_2024_automotive)

  weo_2024_automotive
}


weo_2024_extract_fossil_fuels <- function(weo_2024_fig_chptr_3_raw) {
  weo_2024_oil <- weo_2024_extract_oil(weo_2024_fig_chptr_3_raw)
  weo_2024_gas <- weo_2024_extract_gas(weo_2024_fig_chptr_3_raw)
  weo_2024_coal <- weo_2024_extract_coal(weo_2024_fig_chptr_3_raw)

  weo_2024_fossil_fuels <-
    dplyr::bind_rows(weo_2024_coal, weo_2024_gas, weo_2024_oil) %>%
    dplyr::mutate(
      source = "WEO2024",
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

  pacta.data.validation::validate_intermediate_scenario_output(weo_2024_fossil_fuels)

  weo_2024_fossil_fuels
}

weo_2024_extract_oil <- function(weo_2024_fig_chptr_3_raw) {
  data <-
    weo_2024_fig_chptr_3_raw %>%
    dplyr::filter(.data[["sheet"]] == "Table 3.1") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 12, 31)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 7, 19))

  scenario_headers <-
    dplyr::tibble(
      row = c(11L, 11L, 11L, 11L, 11L),
      col = c(7L, 8L, 9L, 13L, 17L),
      scenario = c("row_names", "start_year", "STEPS", "APS", "NZE")
    )

  data |>
    unpivotr::enhead(scenario_headers, "up-left") |>
    unpivotr::behead("up", "year") |>
    unpivotr::behead("left", "technology") |>
    dplyr::filter(!is.na(numeric)) |>
    dplyr::select("technology", "scenario", "year", value = "numeric") |>
    tidyr::pivot_wider(
      names_from = c("scenario", "year"),
      values_from = "value"
      ) |>
    dplyr::mutate(
      STEPS_2023 = .data[["start_year_2023"]],
      APS_2023 = .data[["start_year_2023"]],
      NZE_2023 = .data[["start_year_2023"]],
    ) |>
    dplyr::select(-"start_year_2023") |>
    tidyr::pivot_longer(
      cols = -"technology",
      names_to = c("scenario", "year"),
      names_sep = "_",
      names_transform = list(year = as.integer)
    ) |>
    dplyr::filter(
      .data[["technology"]] %in% c("World oil supply", "NGLs")
    ) %>%
    tidyr::pivot_wider(
      names_from = "technology",
      values_from = "value"
    ) %>%
    dplyr::rename(
      natural_gas_liquids = "NGLs",
      oil = "World oil supply"
    ) %>%
    dplyr::mutate(value = .data[["oil"]] - .data[["natural_gas_liquids"]]) |>
    dplyr::mutate(
      sector = "Oil&Gas",
      technology = "Oil",
      units = "mb/d"
    ) |>
    dplyr::arrange("technology", "scenario", "year")
}


weo_2024_extract_gas <- function(weo_2024_fig_chptr_3_raw) {
  scenario_headers <-
    dplyr::tibble(
      row = c(10L, 10L, 10L, 10L, 10L),
      col = c(6L, 7L, 9L, 13L, 17L),
      scenario = c("row_names", "start_year", "STEPS", "APS", "NZE")
    )

  weo_2024_fig_chptr_3_raw %>%
    dplyr::filter(.data[["sheet"]] == "Table 3.2") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 10, 33)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 6, 19)) %>%
    dplyr::filter(!all(is_blank), .by = "col") %>%
    unpivotr::enhead(scenario_headers, "up-ish") %>%
    unpivotr::behead("up", "year") %>%
    unpivotr::behead("left", "technology") %>%
    dplyr::select("technology", "scenario", "year", value = "numeric") %>%
    dplyr::filter(.data[["technology"]] == "Natural gas production (bcm)") %>%
    tidyr::pivot_wider(
      names_from = c("scenario", "year"),
      values_from = "value"
      ) %>%
    dplyr::mutate(
      STEPS_2023 = .data[["start_year_2023"]],
      APS_2023 = .data[["start_year_2023"]],
      NZE_2023 = .data[["start_year_2023"]]
    ) %>%
    dplyr::select(-"start_year_2023") %>%
    tidyr::pivot_longer(
      cols = -"technology",
      names_to = c("scenario", "year"),
      names_sep = "_",
      names_transform = list(year = as.integer)
    ) %>%
    dplyr::arrange("technology", "scenario", "year") %>%
    dplyr::mutate(
      sector = "Oil&Gas",
      technology = "Gas",
      units = "bcm"
    )
}


weo_2024_extract_coal <- function(weo_2024_fig_chptr_3_raw) {
  weo_2024_fig_chptr_3_raw %>%
    dplyr::filter(.data[["sheet"]] == "Table 3.3") %>%
    dplyr::filter(dplyr::between(.data[["row"]], 13, 28)) %>%
    dplyr::filter(dplyr::between(.data[["col"]], 5, 17)) %>%
    dplyr::filter(!all(is_blank), .by = "col") %>%
    unpivotr::behead("up-left", "scenario") %>%
    unpivotr::behead("up", "year") %>%
    unpivotr::behead("left", "technology") %>%
    dplyr::select("technology", "scenario", "year", value = "numeric") %>%
    dplyr::filter(.data[["technology"]] == "World coal production") %>%
    tidyr::pivot_wider(
      names_from = c("scenario", "year"),
      values_from = "value") %>%
    dplyr::mutate(
      STEPS_2023 = .data[["NA_2023"]],
      APS_2023 = .data[["NA_2023"]],
      NZE_2023 = .data[["NA_2023"]]
    ) %>%
    dplyr::select(-"NA_2023") %>%
    tidyr::pivot_longer(
      cols = -"technology",
      names_to = c("scenario", "year"),
      names_sep = "_",
      names_transform = list(year = as.integer)
    ) %>%
    dplyr::arrange("technology", "scenario", "year") %>%
    dplyr::mutate(
      sector = "Coal",
      technology = "Coal",
      units = "Mtce"
    )
}


weo_2024_extract_aviation <- function(mpp_ats_raw, weo_2024_ext_data_world_raw) {
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

  weo_2024_extended_world <-
    weo_2024_ext_data_world_raw %>%
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
        .data[["variable"]] == "Annual GHG Emissions in tCO2e - PRU"
      ) %>%
      tidyr::pivot_longer(
        cols = tidyr::matches("20[0-9]{2}$"),
        names_to = "year",
        names_transform = as.integer,
        values_to = "value"
      ) %>%
      dplyr::filter(.data[["value"]] != 0)
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

  weo_2024_aviation_emissions <-
    weo_2024_ext_data_world_raw %>%
    dplyr::rename_with(.fn = tolower) %>%
    dplyr::filter(
      .data[["flow"]] == "Aviation: domestic and international bunkers",
      .data[["category"]] == "CO2 combustion"
    ) %>%
    dplyr::rename(emissions = "value")

  weo_2024_aviation_pkm <-
    weo_2024_ext_data_world_raw %>%
    dplyr::rename_with(.fn = tolower) %>%
    dplyr::filter(
      .data[["flow"]] == "Aviation: domestic and international bunkers",
      .data[["category"]] == "Activity of stock"
    ) %>%
    dplyr::rename(passenger_km = "value")

  # combine aviation data
  weo_2024_aviation <-
    weo_2024_aviation_emissions %>%
    dplyr::left_join(
      weo_2024_aviation_pkm,
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
    dplyr::select("value", "year", "scenario") %>%
    dplyr::mutate(
      source = "WEO2024",
      scenario_geography = "Global",
      units = "tCO2/pkm",
      indicator = "Emission Intensity",
      sector = "Aviation",
      technology = "Passenger",
      scenario = dplyr::case_when(
        scenario == "Stated Policies Scenario" ~ "STEPS",
        scenario == "Announced Pledges Scenario" ~ "APS",
        scenario == "Net Zero Emissions by 2050 Scenario" ~ "NZE"
      )
    )

  pacta.data.validation::validate_intermediate_scenario_output(weo_2024_aviation)

  weo_2024_aviation
}

