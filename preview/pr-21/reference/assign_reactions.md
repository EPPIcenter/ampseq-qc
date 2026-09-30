# Assign targets to reactions

Reads the pools for each target from the `pool` column of the panel
information (targets shared by several pools have comma-separated pools)
and maps them to reactions using the panel settings. A target shared by
pools in different reactions is returned once per reaction, so it counts
towards every reaction it was amplified in.

## Usage

``` r
assign_reactions(panel_information, settings = default_panel_settings())
```

## Arguments

- panel_information:

  Data frame read from `panel_information/amplicon_info.tsv`.

- settings:

  Panel settings, see
  [`panel_settings()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/panel_settings.md).

## Value

`panel_information` with a `reaction` column, one row per target and
reaction.
