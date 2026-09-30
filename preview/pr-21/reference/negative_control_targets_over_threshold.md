# Negative control targets above a read threshold

Negative control targets above a read threshold

## Usage

``` r
negative_control_targets_over_threshold(
  amplicons_negative,
  negative_control_read_threshold = 50
)
```

## Arguments

- amplicons_negative:

  Output of
  [`negative_control_amplicons()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_amplicons.md).

- negative_control_read_threshold:

  Reads above which a target is reported.

## Value

`sample_name`, `target_name` and `OutputPostprocessing` for targets
above the threshold.
