# Prepare ISF 2023 scenario data

Prepare ISF 2023 scenario data

## Usage

``` r
prepare_isf_2023_scenario(
  isf_2023_scope_global_raw,
  isf_2023_s_global_raw,
  isf_2023_annex_countries_raw
)
```

## Arguments

- isf_2023_scope_global_raw:

  A tidyxl data frame (with a `formats` attribute) with a raw ISF Scope
  Global 2023 import.

- isf_2023_s_global_raw:

  A tidyxl data frame (with a `formats` attribute) with a raw ISF
  S_Global 2023 import.

- isf_2023_annex_countries_raw:

  A list of tidyxl data frames (with a `formats` attribute) containing
  the raw import of each of the Annex Countries xlsx files for ISF 2023.

## Value

A prepared ISF 2023 scenario data-frame.
