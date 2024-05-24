#' Minimal explicit datasets that allow overwriting values
#'
#' These funtions are developer-oriented. They all call [tibble()] so
#' you can expect all the goodies that come with that.
#'
#' @section Params
#' The arguments are the column names of the datasets being faked. They all have
#' a default and it can be overwritten.
#'
#' @section Pros and cons
#' These functions help you to avoid duplicating test code, and help
#' the reader of your code to focus on the one thing you want to test, instead
#' of burring that thing in the much longer code you need to create a fake
#' object from scratch.
#'
#' But `fake_*()` functions hide the explicit content. If the reader of your
#' code wants to inspect the data being tested, they need to jump to the
#' function definition or call them interactively.
#'
#' @return A data.frame
#' @noRd
fake_technology_bridge <- function(TechnologyName = NULL,
                                   TechnologyAll = NULL) {
  dplyr::tibble(
    TechnologyName = TechnologyName %||% "Automotive",
    TechnologyAll = TechnologyAll %||% "Cars"
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_geco_2022_automotive_raw <- function(GECO = NULL,
                                          Scenario = NULL,
                                          Vehicle = NULL,
                                          Sector = NULL,
                                          Technology = NULL,
                                          Variable = NULL,
                                          Unit = NULL,
                                          Region = NULL,
                                          `2020` = NULL,
                                          `2025` = NULL) {
  dplyr::tibble(
    GECO = GECO %||% "GECO2022",
    Scenario = Scenario %||% "NDC-LTS",
    Vehicle = Vehicle %||% "Cars",
    Sector = Sector %||% "Light Vehicles",
    Technology = Technology %||% "Electric",
    Variable = Variable %||% "Sales",
    Unit = Unit %||% "k*veh",
    Region = Region %||% "World",
    `2020` = `2020` %||% 12345,
    `2025` = `2025` %||% 54321
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_geco_2022_aviation_raw <- function(GECO = NULL,
                                        Scenario = NULL,
                                        Variable = NULL,
                                        Sector = NULL,
                                        `Passenger/Freight` = NULL,
                                        Technology = NULL,
                                        Unit = NULL,
                                        Region = NULL,
                                        `2020` = NULL,
                                        `2025` = NULL) {
  dplyr::tibble(
    GECO = GECO %||% "GECO2022",
    Scenario = Scenario %||% "NDC-LTS",
    Variable = Variable %||% "Emission Intensity",
    `Passenger/Freight` = `Passenger/Freight` %||% "Passenger",
    Technology = Technology %||% "Passenger",
    Unit = Unit %||% "gCO2/pkm",
    Region = Region %||% "Global",
    `2020` = `2020` %||% 0.54321,
    `2025` = `2025` %||% 0.12345
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_geco_2022_fossil_fuels_raw <- function(GECO = NULL,
                                            Scenario = NULL,
                                            Variable = NULL,
                                            Fuel = NULL,
                                            Unit = NULL,
                                            Region = NULL,
                                            `2020` = NULL,
                                            `2025` = NULL) {
  dplyr::tibble(
    GECO = GECO %||% "GECO2022",
    Scenario = Scenario %||% "1.5C",
    Variable = Variable %||% "Production",
    Fuel = Fuel %||% "Coal",
    Unit = Unit %||% "mtoe",
    Region = Region %||% "World",
    `2020` = `2020` %||% 54321,
    `2025` = `2025` %||% 12345
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_geco_2022_power_raw <- function(GECO = NULL,
                                     Scenario = NULL,
                                     Variable = NULL,
                                     Unit = NULL,
                                     Region = NULL,
                                     Technology = NULL,
                                     `2020` = NULL,
                                     `2025` = NULL) {
  dplyr::tibble(
    GECO = GECO %||% "GECO2022",
    Scenario = Scenario %||% "1.5C",
    Variable = Variable %||% "Capacity",
    Unit = Unit %||% "GW",
    Region = Region %||% "World",
    Technology = Technology %||% "Coal",
    `2020` = `2020` %||% 54321,
    `2025` = `2025` %||% 12345
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_geco_2022_steel_raw <- function(Source = NULL,
                                     Sector = NULL,
                                     Scenario = NULL,
                                     Variable = NULL,
                                     Unit = NULL,
                                     Region = NULL,
                                     Technology = NULL,
                                     `2020` = NULL,
                                     `2025` = NULL) {
  dplyr::tibble(
    Source = Source %||% "GECO2022",
    Sector = Sector %||% "Steel",
    Scenario = Scenario %||% "1.5C",
    Region = Region %||% "Global",
    `2020` = `2020` %||% 5.4321,
    `2025` = `2025` %||% 1.2345
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_format_p4b_input <- function(source = NULL,
                                  scenario = NULL,
                                  scenario_geography = NULL,
                                  sector = NULL,
                                  technology = NULL,
                                  indicator = NULL,
                                  units = NULL,
                                  year = NULL,
                                  tmsr = NULL,
                                  smsp = NULL) {
  dplyr::tibble(
    source = source %||% "WEO2023",
    scenario = scenario %||% "NZE_2050",
    scenario_geography = scenario_geography %||% "Global",
    sector = sector %||% "Oil&Gas",
    technology = technology %||% "Oil",
    indicator = indicator %||% "Production",
    units = units %||% "mb/d",
    year = year %||% 2020,
    tmsr = tmsr %||% 5.4321,
    smsp = smsp %||% 1.2345
  )
}
#' See `fake_technology_bridge()`
#' @noRd
fake_format_p4b_ei_input <- function(source = NULL,
                                     scenario = NULL,
                                     scenario_geography = NULL,
                                     sector = NULL,
                                     technology = NULL,
                                     indicator = NULL,
                                     units = NULL,
                                     year = NULL,
                                     value = NULL) {
  dplyr::tibble(
    source = source %||% "WEO2023",
    scenario = scenario %||% "NZE_2050",
    scenario_geography = scenario_geography %||% "Global",
    sector = sector %||% "Steel",
    technology = technology %||% NA_character_,
    indicator = indicator %||% "Emission Intensity",
    units = units %||% "tCO2/t Steel",
    year = year %||% 2020,
    value = value %||% 5.4321
  )
}
