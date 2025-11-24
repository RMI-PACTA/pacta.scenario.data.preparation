# A dataset that maps scenario regions as defined by their source, to a list of PACTA compatible scenario regions.

Scenario providers often define their own, tailor-made lists of what
countries form a region. The entire concept of a region is not
standardized and can even change year on year. However, for the purpose
of the PACTA transition monitor website, it is useful to have a minimal
set of comparable regions, to cycle through different scenarios for.
Right now, these regions are the "Global" region, which contains all
countries, and the "OECD" and "NonOECD" regions.

This dataset provides a bridge between whatever the scenario has
labelled these regions as (e.g. "WORLD"), and the terminology that PACTA
uses (e.g. "Global").

## Usage

``` r
scenario_source_pacta_geography_bridge
```

## Format

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 15 rows and 3 columns.

## Examples

``` r
head(scenario_source_pacta_geography_bridge)
#> # A tibble: 6 × 3
#>   source   scenario_geography_source scenario_geography_pacta
#>   <chr>    <chr>                     <chr>                   
#> 1 ETP2017  Global                    Global                  
#> 2 ETP2020  Global                    Global                  
#> 3 GECO2021 Global                    Global                  
#> 4 ISF2021  Global                    Global                  
#> 5 WEO2021  Global                    Global                  
#> 6 WEO2021  OECD                      OECD                    
```
