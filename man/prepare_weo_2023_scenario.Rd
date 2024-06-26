% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_weo_2023_scenario.R
\name{prepare_weo_2023_scenario}
\alias{prepare_weo_2023_scenario}
\title{Prepare WEO 2023 scenario data}
\usage{
prepare_weo_2023_scenario(
  weo_2023_ext_data_regions_raw,
  weo_2023_ext_data_world_raw,
  weo_2023_fig_chptr_3_raw,
  iea_global_ev_raw,
  mpp_ats_raw
)
}
\arguments{
\item{weo_2023_ext_data_regions_raw}{A data frame containing a raw
\code{WEO2023_Extended_Data_Regions.csv} import.}

\item{weo_2023_ext_data_world_raw}{A data frame containing a raw
\code{WEO2023_Extended_Data_World.csv} import.}

\item{weo_2023_fig_chptr_3_raw}{A tidyxl data frame containing a raw import
of \code{WEO2023_Figures_Chapter_03.xlsx}.}

\item{iea_global_ev_raw}{A data frame containing a raw \verb{IEA Global EV Data 2023.csv} import.}

\item{mpp_ats_raw}{A tidyxl data frame containing a raw import of `2022-08-12
\itemize{
\item MPP ATS - RPK and GHG intensity.xlsx`.
}}
}
\value{
A prepared WEO 2023 scenario data-frame.
}
\description{
Prepare WEO 2023 scenario data
}
