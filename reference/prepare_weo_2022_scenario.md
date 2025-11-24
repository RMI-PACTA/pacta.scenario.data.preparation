# Prepare WEO 2022 scenario data

Prepare WEO 2022 scenario data

## Usage

``` r
prepare_weo_2022_scenario(
  weo_2022_ext_data_regions_raw,
  weo_2022_ext_data_world_raw,
  weo_2022_fossil_fuels_raw,
  weo_2022_nze_auto_raw,
  weo_2022_nze_steel_raw,
  weo_2022_sales_aps_auto_raw,
  weo_2022_electric_sales_aps_auto_raw
)
```

## Arguments

- weo_2022_ext_data_regions_raw:

  A data frame containing a raw import of
  `WEO2022_Extended_Data_Regions.csv`.

- weo_2022_ext_data_world_raw:

  A data frame containing a raw import of
  `WEO2022_Extended_Data_World.csv`.

- weo_2022_fossil_fuels_raw:

  A data frame containing a raw import of
  `weo2022_fossilfuel_demand_supply.csv`.

- weo_2022_nze_auto_raw:

  A tidyxl data frame with a raw `NZE2021_RawData_2050.xlsx` import.

- weo_2022_nze_steel_raw:

  A data frame containing a raw import of `WEO2022_NZE_SteelData.csv`.

- weo_2022_sales_aps_auto_raw:

  A data frame containing a raw import of `SalesAPS_rawdata.csv`.

- weo_2022_electric_sales_aps_auto_raw:

  A data frame containing a raw import of
  `IEA-EV-dataEV salesCarsProjection-APS.csv`.

## Value

A prepared WEO 2022 scenario data-frame.
