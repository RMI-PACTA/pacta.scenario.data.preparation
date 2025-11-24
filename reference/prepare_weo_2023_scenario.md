# Prepare WEO 2023 scenario data

Prepare WEO 2023 scenario data

## Usage

``` r
prepare_weo_2023_scenario(
  weo_2023_ext_data_regions_raw,
  weo_2023_ext_data_world_raw,
  weo_2023_fig_chptr_3_raw,
  iea_global_ev_raw,
  mpp_ats_raw
)
```

## Arguments

- weo_2023_ext_data_regions_raw:

  A data frame containing a raw `WEO2023_Extended_Data_Regions.csv`
  import.

- weo_2023_ext_data_world_raw:

  A data frame containing a raw `WEO2023_Extended_Data_World.csv`
  import.

- weo_2023_fig_chptr_3_raw:

  A tidyxl data frame containing a raw import of
  `WEO2023_Figures_Chapter_03.xlsx`.

- iea_global_ev_raw:

  A data frame containing a raw `IEA Global EV Data 2023.csv` import.

- mpp_ats_raw:

  A tidyxl data frame containing a raw import of \`2022-08-12

  - MPP ATS - RPK and GHG intensity.xlsx\`.

## Value

A prepared WEO 2023 scenario data-frame.
