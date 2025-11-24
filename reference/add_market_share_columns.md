# Add market share columns to a scenario dataset

Calculates and adds market share values (ie. technology market-share
ratio and sector market-share percentage) to a scenario dataset. A
reference start-year must be provided.

## Usage

``` r
add_market_share_columns(data, reference_year)
```

## Arguments

- data:

  A scenario dataset, like FIXME: Define an exported demo scenario.

- reference_year:

  The baseline year, against which the technology- and sector- market
  shares will be calculated. Note: At the start year, tmsr = 1 and smsp
  = 0 respectively.

## Value

A scenario dataset, with the new columns `tmsr` and `smsp`.
