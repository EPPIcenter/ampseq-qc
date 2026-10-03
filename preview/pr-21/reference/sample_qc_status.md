# Sample-level QC status

Combines the per-reaction calls into one status per sample, taking the
worst status across reactions (`reprep`, then `repool`, then `pass`).
Positive and negative controls are excluded.

## Usage

``` r
sample_qc_status(reprep_repool_summary)
```

## Arguments

- reprep_repool_summary:

  Output of
  [`fill_missing_data()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/fill_missing_data.md).

## Value

One row per sample with `sample_name`, `Batch` and `status`.
