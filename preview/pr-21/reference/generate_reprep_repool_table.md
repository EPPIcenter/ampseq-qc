# Classify samples as pass, repool or reprep

Negative controls are not classified.

## Usage

``` r
generate_reprep_repool_table(
  summary_samples,
  reprep_threshold = 0.5,
  repool_threshold = 0.75
)
```

## Arguments

- summary_samples:

  Output of
  [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md).

- reprep_threshold:

  Proportion of targets amplified below which a sample needs re-prep.

- repool_threshold:

  Proportion of targets amplified below which a sample needs re-pool.

## Value

One row per sample and reaction with `status` and `reason`.
