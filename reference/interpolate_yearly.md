# Interpolate values in a dataset, by year. Interpolate values in a dataset, by year.

Interpolate values in a dataset, by year. Interpolate values in a
dataset, by year.

## Usage

``` r
interpolate_yearly(data, ...)
```

## Arguments

- data:

  An input dataset. Must contain the columns `year` and `value`.

- ...:

  Other grouping variables. `value` will be interpolated for each group.

## Value

A dataset with the column `value` interpolated linearly against the
column `year`.
