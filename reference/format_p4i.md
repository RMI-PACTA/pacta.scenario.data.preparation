# Format scenario data for P4I

Format scenario data for P4I

## Usage

``` r
format_p4i(data, green_techs)
```

## Arguments

- data:

  A scenario dataset.

- green_techs:

  A list of green technologies. For these, a `direction` of "increasing"
  will be assigned, and the `smsp` column will be used to assign a
  `fair_share_perc`. Otherwise the `direction` will be `decreasing` and
  the `tmsr` column will be used.

## Value

A scenario dataset, with columns renamed to be consistent with
pacta.data.preparation input requirements.
