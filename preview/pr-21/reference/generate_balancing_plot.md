# Balancing across batches

Balancing across batches

## Usage

``` r
generate_balancing_plot(
  summary_samples,
  sample_colours = default_sample_colours
)
```

## Arguments

- summary_samples:

  Output of
  [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md).

- sample_colours:

  Named colours for each sample type.

## Value

A ggplot of total reads per sample by batch.
