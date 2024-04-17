#' Prepare WEO 2022 scenario data
#'
#' @param weo_2022_ext_data_regions_raw A data frame containing a raw import of
#'   `WEO2022_Extended_Data_Regions.csv`.
#' @param weo_2022_ext_data_world_raw A data frame containing a raw import of
#'   `WEO2022_Extended_Data_World.csv`.
#' @param weo_2022_fossil_fuels_raw A data frame containing a raw import of
#'   `weo2022_fossilfuel_demand_supply.csv`.
#' @param weo_2022_nze_auto_raw A tidyxl data frame with a raw
#'   `NZE2021_RawData_2050.xlsx` import.
#' @param weo_2022_nze_steel_raw A data frame containing a raw import of
#'   `WEO2022_NZE_SteelData.csv`.
#' @param weo_2022_sales_aps_auto_raw A data frame containing a raw import of
#'   `SalesAPS_rawdata.csv`.
#' @param weo_2022_electric_sales_aps_auto_raw_text A vector of character
#'   strings containing each line of a raw import of `IEA-EV-dataEV
#'   salesCarsProjection-APS.csv`.
#'
#' @return A prepared WEO 2022 scenario data-frame.
#'
#' @importFrom dplyr %>%
#'
#' @export

prepare_weo_2022_scenario <- function(weo_2022_ext_data_regions_raw,
                                      weo_2022_ext_data_world_raw,
                                      weo_2022_fossil_fuels_raw,
                                      weo_2022_nze_auto_raw,
                                      weo_2022_nze_steel_raw,
                                      weo_2022_sales_aps_auto_raw,
                                      weo_2022_electric_sales_aps_auto_raw_text) {
  lines_to_fix <- 3:length(weo_2022_electric_sales_aps_auto_raw_text)
  weo_2022_electric_sales_aps_auto_raw_text[lines_to_fix] <-
    paste0(
      weo_2022_electric_sales_aps_auto_raw_text[lines_to_fix],
      ",https://www.iea.org/reports/global-ev-outlook-2022/executive-summary"
    )
  weo_2022_electric_sales_aps_auto_raw <-
    readr::read_csv(
      file = I(weo_2022_electric_sales_aps_auto_raw_text),
      show_col_types = FALSE
    )

  fossil_fuel <-
    weo_2022_fossil_fuels_raw %>%
    dplyr::filter(.data[["Variable"]] == "Supply")

  # A bit of manual calculations here, done on excel
  NZE2050_auto <-
    weo_2022_nze_auto_raw %>%
    unpivotr::behead("left", "Source") %>%
    unpivotr::behead("left", "Indicator") %>%
    unpivotr::behead("left", "Sector") %>%
    unpivotr::behead("left", "Units") %>%
    unpivotr::behead("left", "Scenario") %>%
    unpivotr::behead("left", "ScenarioGeography") %>%
    unpivotr::behead("left", "Technology") %>%
    unpivotr::behead("left", "Sub_Technology") %>%
    unpivotr::behead("up", "year") %>%
    dplyr::select("numeric", "Source":dplyr::last_col()) %>%
    tidyr::pivot_wider(names_from = "year", values_from = "numeric") %>%
    dplyr::filter(.data[["Sector"]] == "Automotive")

  NZE_EI <- weo_2022_nze_steel_raw %>% dplyr::select(-"Link")

  sales_APS_auto <- weo_2022_sales_aps_auto_raw

  electric_sales_APS_auto <-
    weo_2022_electric_sales_aps_auto_raw %>%
    dplyr::filter(.data[["region"]] == "World") %>%
    dplyr::mutate(value = .data[["value"]] / 1000000) %>%
    dplyr::mutate(unit = "# (in million)") %>%
    dplyr::mutate(year = as.integer(.data[["year"]]))


  # format scenario region -----------------------------------------------------

  techs_out_of_pacta_scope <- c(
    # All not in PACTA scope + Renewables to avoid double counting
    "Total",
    "Battery storage",
    "Modern biomass",
    "Traditional use of biomass",
    "Total liquids",
    "Electricity",
    "Renewables",
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


  # format scenario_region Power -----------------------------------------------

  scenario_region_clean_names <- janitor::clean_names(weo_2022_ext_data_regions_raw)

  scenario_region <-
    scenario_region_clean_names %>%
    dplyr::rename(
      source = "publication",
      variable = "category",
      technology = "product",
    ) %>%
    dplyr::mutate(
      flow = dplyr::if_else(.data[["unit"]] %in% c("GW","TWh"), "Power", "Fossil Fuels")
    ) %>%
    dplyr::filter(.data[["unit"]] == "GW") %>%
    dplyr::rename(sector = "flow") %>%
    dplyr::filter(!(.data[["technology"]] %in% techs_out_of_pacta_scope)) %>%
    # if we don't do this, Oil capacity goes under Oil&Gas sector
    dplyr::mutate(technology = dplyr::if_else(.data[["technology"]] == "Oil", "OilCap", .data[["technology"]]))

  scenario_region_steps_2021 <-
    scenario_region %>%
    dplyr::filter(.data[["year"]] == 2021) %>%
    dplyr::mutate(scenario = "Stated Policies Scenario")

  scenario_region <-
    dplyr::bind_rows(
      scenario_region,
      scenario_region_steps_2021
    )

  scenario_region_aps <-
    scenario_region %>%
    dplyr::filter(.data[["year"]] < 2030) %>%
    dplyr::filter(.data[["scenario"]] == "Stated Policies Scenario") %>%
    dplyr::mutate(scenario = "Announced Pledges Scenario")

  scenario_region_nze <-
    scenario_region %>%
    dplyr::filter(.data[["year"]] < 2030) %>%
    dplyr::filter(
      .data[["scenario"]] == "Stated Policies Scenario",
      .data[["region"]] == "Advanced economies"
    ) %>%
    dplyr::mutate(scenario = "Net Zero Emissions by 2050 Scenario")

  scenario_region <-
    dplyr::bind_rows(
      scenario_region,
      scenario_region_aps,
      scenario_region_nze
    ) %>%
    dplyr::filter(!is.na(.data[["scenario"]]))


  # format scenario_region_world Power -----------------------------------------

  scenario_region_world_cleaned_names <-
    janitor::clean_names(weo_2022_ext_data_world_raw)

  scenario_region_world <-
    scenario_region_world_cleaned_names %>%
    dplyr::rename(
      source = "publication",
      variable = "category",
      technology = "product",
    ) %>%
    dplyr::mutate(
      flow = dplyr::if_else(.data[["unit"]] %in% c("GW","TWh"), "Power", "Fossil Fuels")
    ) %>%
    dplyr::filter(.data[["unit"]] == "GW") %>%
    dplyr::rename(sector = "flow") %>%
    dplyr::filter(!(.data[["technology"]] %in% techs_out_of_pacta_scope)) %>%
    #if we don't do this, Oil capacity goes under Oil&Gas sector
    dplyr::mutate(technology = dplyr::if_else(.data[["technology"]] == "Oil", "OilCap", .data[["technology"]])) %>%
    dplyr::filter(.data[["sector"]] == "Power")


  # format scenario FF ---------------------------------------------------------

  fossil_fuel_formatted <-
    fossil_fuel %>%
    # we need to remove Natural Gas Liquids from Oil so it needs "special treatment"
    dplyr::filter(.data[["Sector"]] != "Oil") %>%
    dplyr::select(-c("2015", "2020", "2035", "2040", "2045")) %>% # we don't have data for those year
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      year = as.numeric(.data[["year"]]),
      value = as.numeric(.data[["value"]])
    ) %>%
    dplyr::rename(
      source = "Source",
      sector = "Sector",
      technology = "Technology",
      unit = "Units",
      region = "Scenario geography",
      scenario = "Scenario",
      variable = "Variable"
    ) %>%
    dplyr::select(-"Data Source")

  oil_formatted <-
    fossil_fuel %>%
    # we need to remove Natural Gas Liquids from Oil so it needs "special treatment"
    dplyr::filter(.data[["Sector"]] == "Oil") %>%
    dplyr::select(-c("2015", "2020", "2035", "2040", "2045")) %>% # we don't have data for those year
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from = "Technology",
      values_from = "value"
    ) %>%
    dplyr::rename(natural_gas_liquids = "Natural gas liquids",
           oil = "Oil") %>%
    dplyr::mutate(value = .data[["oil"]] - .data[["natural_gas_liquids"]] )%>%
    dplyr::mutate(technology = "Oil") %>%
    dplyr::rename(source = "Source",
           sector = "Sector",
           unit = "Units",
           region = "Scenario geography",
           scenario = "Scenario",
           variable = "Variable") %>%
    dplyr::select(-c("Data Source", "oil", "natural_gas_liquids"))


  # combine and format ---------------------------------------------------------

  renewable_techs <-
    c(
      "Concentrating solar power",
      "Geothermal",
      "Marine",
      "Modern bioenergy and renewable waste",
      "Solar PV",
      "Wind"
    )

  scen_joined <-
    dplyr::bind_rows(
      oil_formatted,
      fossil_fuel_formatted,
      scenario_region,
      scenario_region_world
    ) %>%
    # for granular pathway, we miss some sub technology in renewables and we
    # can't sum them to have renewables total capacities
    dplyr::filter(!.data[["technology"]] %in% renewable_techs | .data[["region"]] == "World")

  # add regional renewables pathway
  # if we sum all sub technology, we miss still small one as geothermical or
  # solar cpv
  # we'll obtain renewables capacities by substractinghydro to total renewables
  # (total renewables contains hydro)

  renewables_region <-
    scenario_region_clean_names %>%
    dplyr::rename(
      source = "publication",
      variable = "category",
      technology = "product",
    ) %>%
    dplyr::mutate(
      flow = dplyr::if_else(.data[["unit"]] %in% c("GW","TWh"), "Power", "Fossil Fuels")
    ) %>%
    dplyr::filter(.data[["unit"]] == "GW") %>%
    dplyr::rename(sector = "flow") %>%
    dplyr::filter(.data[["technology"]] %in% c("Hydro", "Renewables")) %>%
    tidyr::pivot_wider(names_from = "technology", values_from = "value") %>%
    dplyr::mutate(value = .data[["Renewables"]] - .data[["Hydro"]]) %>%
    dplyr::mutate(technology = "RenewablesCap") %>%
    dplyr::mutate(scenario = dplyr::if_else(is.na(.data[["scenario"]]), "Stated Policies Scenario", .data[["scenario"]])) %>%
    dplyr::select("source", "scenario", "variable", "sector", "unit", "region", "year","value","technology")

  renewables_region_baseline_aps <-
    renewables_region %>%
    dplyr::filter(.data[["year"]] < 2030) %>%
    dplyr::mutate(scenario = "Announced Pledges Scenario")

  renewables_region_baseline_nze <-
    renewables_region %>%
    dplyr::filter(.data[["year"]] < 2030) %>% # we miss baseline for NZE and APS
    dplyr::filter(.data[["region"]] == "Advanced economies") %>% # only region where we get granular pathway for NZE2050
    dplyr::mutate(scenario = "Net Zero Emissions by 2050 Scenario")

  scenario_region_total <-
    dplyr::bind_rows(
      scen_joined,
      renewables_region,
      renewables_region_baseline_aps,
      renewables_region_baseline_nze
    )

  scen_complete <-
    scenario_region_total %>%
    # scen_joined_old %>%
    dplyr::rename(
      indicator = "variable",
      scenario_geography = "region",
      units = "unit"
    ) %>%
    dplyr::mutate(year = as.double(.data[["year"]]))

  scen_total <-
    scen_complete %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data[["scenario_geography"]] == "Emerging market & developing economies",
        "Emerging market and developing economies",
        .data[["scenario_geography"]]
      )
    ) %>%
    dplyr::mutate(
      scenario = dplyr::case_when(
        .data[["scenario"]] == "Announced Pledges Scenario" ~ "APS",
        .data[["scenario"]] == "Sustainable Development Scenario" ~ "SDS",
        .data[["scenario"]] == "Stated Policies Scenario" ~ "STEPS",
        TRUE ~ .data[["scenario"]],
      )
    ) %>%
    dplyr::mutate(
      sector = dplyr::case_when(
        .data[["technology"]] == "Coal" ~ "Coal",
        .data[["technology"]] == "Oil" ~ "Oil&Gas",
        .data[["technology"]] == "Gas" ~ "Oil&Gas",
        TRUE ~ .data[["sector"]]
      )
    )

  # Issue - scope are not the same for CoalCap and GasCap
  # global includes CCUS in the forecast
  # regional don't include CCUS in the forecast


  # process automotive sector --------------------------------------------------
  # Add NZE scenario for Auto from May 2021 release
  # Data not updated since that report - confirmation from WEO this data remain
  # accurate for WEO2022

  nze2050_auto_cleaned <- janitor::clean_names(NZE2050_auto)

  nze2050_auto_long <-
    nze2050_auto_cleaned %>%
    dplyr::select(-"sub_technology") %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("x20[0-9]{2}$"),
      names_to = "year",
      names_prefix = "x",
      names_transform = list(Year = as.numeric),
      values_to = "value",
      values_ptypes = numeric()
    ) %>%
    dplyr::mutate(year = as.double(.data[["year"]]))  %>%
    dplyr::filter(.data[["year"]] <= 2030) %>% # no scenario data after 2030 - not hard coded
    dplyr::mutate(value = .data[["value"]] / 1000000) %>%
    dplyr::mutate(units = "# (in million)")

  # Add APS and STEPS data for Automotive

  sales_APS_auto_final <-
    sales_APS_auto %>%
    dplyr::filter(.data[["Technology"]] != "EV") %>%   # EV contains Battery Electric Vehicles (what we call EV in AI database, but also Plug In Hybrid , what we call Hybrid)
    tidyr::pivot_longer(
      cols = dplyr::matches("20[0-9]{2}$"),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::rename(source = "Source") %>%
    dplyr::rename(technology = "Technology") %>%
    dplyr::mutate(sector = "Automotive") %>%
    dplyr::mutate(scenario_geography = "Global") %>%
    dplyr::mutate(scenario = "APS") %>%
    dplyr::mutate(source = "WEO2022") %>%
    dplyr::mutate(units = "# (in million)") %>%
    dplyr::mutate(indicator = "Sales") %>%
    dplyr::mutate(year = as.double(.data[["year"]]))

  electric_sales_APS_auto_final <-
    electric_sales_APS_auto %>%
    dplyr::rename(technology = "powertrain") %>%
    dplyr::mutate(sector = "Automotive") %>%
    dplyr::mutate(scenario_geography = "Global") %>%
    dplyr::mutate(scenario = "APS") %>%
    dplyr::mutate(source = "WEO2022") %>%
    dplyr::mutate(units = "# (in million)") %>%
    dplyr::mutate(indicator = "Sales") %>%
    dplyr::select("source","scenario","scenario_geography","sector","technology","indicator","units","year","value")


  # process Cement and Steel sectors -------------------------------------------

  # Add NZE Emission intensity for Steel and Cement
  nze_ei_cleaned <- janitor::clean_names(NZE_EI)

  nze_ei_long <-
    nze_ei_cleaned %>%
    dplyr::mutate(source = "NZE2021") %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("x20[0-9]{2}$"),
      names_to = "year",
      names_prefix = "x",
      names_transform = list(year = as.numeric),
      values_to = "value",
      values_ptypes = numeric()
    ) %>%
    dplyr::mutate(year = as.double(.data[["year"]])) %>%
    dplyr::rename(
      indicator = "variable",
      units = "unit"
    ) %>%
    dplyr::mutate(
      indicator = stringr::str_to_title(.data$indicator)
    )

  weo_2022_combined <-
    dplyr::bind_rows(
      scen_total,
      nze2050_auto_long,
      nze_ei_long,
      electric_sales_APS_auto_final,
      sales_APS_auto_final
    )

  weo_2022_harmonized_geographies <-
    weo_2022_combined %>%
    dplyr::mutate(source = "WEO2022") %>%
    dplyr::mutate(scenario = dplyr::if_else(
      .data[["scenario"]] == "Net Zero Emissions by 2050 Scenario",
      "NZE_2050",
      .data[["scenario"]]
      )
    )%>%
    dplyr::mutate(
      technology = gsub("oil", "Oil", .data[["technology"]]),
      technology = gsub("gas", "Gas", .data[["technology"]]),
      technology = gsub("coal", "Coal", .data[["technology"]]),
      technology = gsub("cap", "Cap", .data[["technology"]]),
      technology = gsub("hybrid", "Hybrid", .data[["technology"]]),
      technology = gsub("electric", "Electric", .data[["technology"]]),
      technology = gsub("ice", "ICE", .data[["technology"]]),
      technology = gsub("fuelcell", "FuelCell", .data[["technology"]]),
      technology = gsub("renewables", "Renewables", .data[["technology"]]),
      technology = gsub("hydro", "Hydro", .data[["technology"]]),
      technology = gsub("nuclear", "Nuclear", .data[["technology"]]),
      sector = gsub("oil&gas", "Oil&Gas", .data[["sector"]]),
      sector = gsub("coal", "Coal", .data[["sector"]]),
      sector = gsub("power", "Power", .data[["sector"]]),
      sector = gsub("automotive", "Automotive", .data[["sector"]]),
      sector = gsub("heavy-duty vehicles", "HDV", .data[["sector"]])
    ) %>%
    dplyr::mutate(
      scenario_geography = dplyr::if_else(
        .data[["scenario_geography"]] == "World",
        "Global",
        .data[["scenario_geography"]]
      )
    )
  # OPEC/NonOPEC not included yet as GlobalAggregate not for O&G yet, Advanced
  # Economies and Emerging market too they are subgroup of region and can't be
  # one as such

  out <-
    weo_2022_harmonized_geographies %>%
    bridge_technologies(weo_2022_technology_bridge) %>%
    bridge_geographies(weo_2022_geography_bridge) %>%
    dplyr::summarise(value = sum(.data[["value"]]), .by = -"value") %>%
    dplyr::arrange(
      .data[["source"]],
      .data[["scenario"]],
      .data[["scenario_geography"]],
      .data[["sector"]],
      .data[["technology"]],
      .data[["indicator"]],
      .data[["units"]],
      .data[["year"]]
    ) %>%
    dplyr::relocate(
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

  pacta.data.validation::validate_intermediate_scenario_output(out)

  out
}
