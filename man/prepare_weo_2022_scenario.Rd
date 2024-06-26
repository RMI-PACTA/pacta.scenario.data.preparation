% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_weo_2022_scenario.R
\name{prepare_weo_2022_scenario}
\alias{prepare_weo_2022_scenario}
\title{Prepare WEO 2022 scenario data}
\usage{
prepare_weo_2022_scenario(
  weo_2022_ext_data_regions_raw,
  weo_2022_ext_data_world_raw,
  weo_2022_fossil_fuels_raw,
  weo_2022_nze_auto_raw,
  weo_2022_nze_steel_raw,
  weo_2022_sales_aps_auto_raw,
  weo_2022_electric_sales_aps_auto_raw
)
}
\arguments{
\item{weo_2022_ext_data_regions_raw}{A data frame containing a raw import of
\code{WEO2022_Extended_Data_Regions.csv}.}

\item{weo_2022_ext_data_world_raw}{A data frame containing a raw import of
\code{WEO2022_Extended_Data_World.csv}.}

\item{weo_2022_fossil_fuels_raw}{A data frame containing a raw import of
\code{weo2022_fossilfuel_demand_supply.csv}.}

\item{weo_2022_nze_auto_raw}{A tidyxl data frame with a raw
\code{NZE2021_RawData_2050.xlsx} import.}

\item{weo_2022_nze_steel_raw}{A data frame containing a raw import of
\code{WEO2022_NZE_SteelData.csv}.}

\item{weo_2022_sales_aps_auto_raw}{A data frame containing a raw import of
\code{SalesAPS_rawdata.csv}.}

\item{weo_2022_electric_sales_aps_auto_raw}{A data frame containing a raw
import of \verb{IEA-EV-dataEV salesCarsProjection-APS.csv}.}
}
\value{
A prepared WEO 2022 scenario data-frame.
}
\description{
Prepare WEO 2022 scenario data
}
