# Overall QC counts

Counts samples (excluding positive and negative controls) by their
sample-level status from
[`sample_qc_status()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/sample_qc_status.md),
so each sample is counted once.

## Usage

``` r
qc_status_counts(reprep_repool_summary)
```

## Arguments

- reprep_repool_summary:

  Output of
  [`fill_missing_data()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/fill_missing_data.md).

## Value

A list with `reprep`, `repool`, `pass`, `total` and `percentage_pass`
(`NA` if there are no samples).
