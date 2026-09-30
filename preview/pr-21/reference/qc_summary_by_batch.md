# QC status by batch

QC status by batch

## Usage

``` r
qc_summary_by_batch(reprep_repool_summary)

qc_summary_table(reprep_repool_summary)
```

## Arguments

- reprep_repool_summary:

  Output of
  [`fill_missing_data()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/fill_missing_data.md).

## Value

`qc_summary_by_batch()` returns counts and percentages per batch and
status (excluding controls). `qc_summary_table()` returns one row per
batch with pass, repool and reprep counts and the pass rate.
