# Primer dimer content

Primer dimer content

## Usage

``` r
generate_dimer_plot(
  sample_coverage_with_manifest,
  sample_colours = default_sample_colours
)
```

## Arguments

- sample_coverage_with_manifest:

  Output of
  [`merge_sample_coverage()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/merge_sample_coverage.md).

- sample_colours:

  Named colours for each sample type.

## Value

A ggplot of input reads against percentage of dimers per batch.
