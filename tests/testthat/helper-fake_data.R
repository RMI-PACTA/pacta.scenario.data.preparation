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
  tibble::tibble(
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
  tibble::tibble(
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
                                        `Passenger/Frieght` = NULL,
                                        Technology = NULL,
                                        Unit = NULL,
                                        Region = NULL,
                                        `2020` = NULL,
                                        `2025` = NULL) {
  tibble::tibble(
    GECO = GECO %||% "GECO2022",
    Scenario = Scenario %||% "NDC-LTS",
    Variable = Variable %||% "Emission Intensity",
    `Passenger/Frieght` = `Passenger/Frieght` %||% "Passenger",
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
  tibble::tibble(
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
  tibble::tibble(
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
  tibble::tibble(
    Source = Source %||% "GECO2022",
    Sector = Sector %||% "Steel",
    Scenario = Scenario %||% "1.5C",
    Region = Region %||% "Global",
    `2020` = `2020` %||% 5.4321,
    `2025` = `2025` %||% 1.2345
  )
}
