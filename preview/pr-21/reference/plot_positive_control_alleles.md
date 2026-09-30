# Alleles per target in positive controls

Alleles per target in positive controls

## Usage

``` r
plot_positive_control_alleles(category_counts)
```

## Arguments

- category_counts:

  `category_counts` from
  [`summarise_positive_controls()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_positive_controls.md).

## Value

A ggplot of targets per allele category for each positive control, with
a `text` aesthetic for
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
tooltips.
