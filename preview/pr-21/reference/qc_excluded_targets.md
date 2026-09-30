# Targets excluded from QC

Combines the targets excluded in the panel settings with long targets
(insert plus primers longer than `long_target_threshold`), which are
known to perform less well.

## Usage

``` r
qc_excluded_targets(
  panel_information,
  settings = default_panel_settings(),
  long_target_threshold = 275
)
```

## Arguments

- panel_information:

  Data frame read from `panel_information/amplicon_info.tsv`.

- settings:

  Panel settings, see
  [`panel_settings()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/panel_settings.md).

- long_target_threshold:

  Length in bp, including primers, above which a target is considered
  long.

## Value

A list with `excluded` (all excluded target names) and `long` (the long
target names).
