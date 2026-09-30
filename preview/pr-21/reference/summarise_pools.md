# Summarise pools in a run

Summarise pools in a run

## Usage

``` r
summarise_pools(panel_information, settings = default_panel_settings())
```

## Arguments

- panel_information:

  Data frame read from `panel_information/amplicon_info.tsv`.

- settings:

  Panel settings, see
  [`panel_settings()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/panel_settings.md).

## Value

A data frame with one row per pool giving its reaction and number of
targets.
