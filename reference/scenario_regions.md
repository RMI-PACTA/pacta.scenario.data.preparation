# A dataset of countries contained in different scenario regions.

This dataset contains a map of each scenario region name, along with all
countries defined within that region (as per the country's ISO2 code).

## Usage

``` r
scenario_regions
```

## Format

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 1492 rows and 4 columns.

## Examples

``` r
head(scenario_regions)
#> # A tibble: 6 × 4
#>   scenario_geography country        country_iso reg_count
#>   <chr>              <chr>          <chr>           <int>
#> 1 Global             Afghanistan    AF                281
#> 2 Global             Albania        AL                281
#> 3 Global             Algeria        DZ                281
#> 4 Global             American Samoa AS                281
#> 5 Global             Andorra        AD                281
#> 6 Global             Angola         AO                281
```
