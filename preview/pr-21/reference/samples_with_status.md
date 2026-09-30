# Samples with a given QC status

Samples with a given QC status

## Usage

``` r
samples_with_status(reprep_repool_summary, qc_status)
```

## Arguments

- reprep_repool_summary:

  Output of
  [`fill_missing_data()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/fill_missing_data.md).

- qc_status:

  `"repool"` or `"reprep"`.

## Value

The rows with that status, sorted by batch, sample and reaction.
