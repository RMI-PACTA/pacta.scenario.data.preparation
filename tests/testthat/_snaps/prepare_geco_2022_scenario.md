# with known inputs, has expected output

    Code
      out
    Output
      # A tibble: 18 x 9
         source   scenario  scenario_geography sector technology indicator units  year
         <chr>    <chr>     <chr>              <chr>  <chr>      <chr>     <chr> <dbl>
       1 GECO2022 1.5C      Global             Power  CoalCap    Capacity  GW     2020
       2 GECO2022 1.5C      Global             Power  CoalCap    Capacity  GW     2025
       3 GECO2022 NDC_LTS   Global             Power  CoalCap    Capacity  GW     2020
       4 GECO2022 NDC_LTS   Global             Power  CoalCap    Capacity  GW     2025
       5 GECO2022 Reference Global             Power  CoalCap    Capacity  GW     2020
       6 GECO2022 Reference Global             Power  CoalCap    Capacity  GW     2025
       7 GECO2022 NDC_LTS   Global             HDV    Electric   Sales     k*veh  2020
       8 GECO2022 NDC_LTS   Global             HDV    Electric   Sales     k*veh  2025
       9 GECO2022 1.5C      Global             Coal   Coal       Producti~ mtoe   2020
      10 GECO2022 1.5C      Global             Coal   Coal       Producti~ mtoe   2025
      11 GECO2022 NDC_LTS   Global             Coal   Coal       Producti~ mtoe   2020
      12 GECO2022 NDC_LTS   Global             Coal   Coal       Producti~ mtoe   2025
      13 GECO2022 Reference Global             Coal   Coal       Producti~ mtoe   2020
      14 GECO2022 Reference Global             Coal   Coal       Producti~ mtoe   2025
      15 GECO2022 NDC_LTS   Global             Aviat~ Passenger  Emission~ tCO2~  2020
      16 GECO2022 NDC_LTS   Global             Aviat~ Passenger  Emission~ tCO2~  2025
      17 GECO2022 1.5C      Global             Steel  <NA>       Emission~ tCO2~  2020
      18 GECO2022 1.5C      Global             Steel  <NA>       Emission~ tCO2~  2025
      # i 1 more variable: value <dbl>

