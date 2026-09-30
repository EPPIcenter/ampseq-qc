# Sample reads against amplification success

Sample reads against amplification success

## Usage

``` r
generate_reads_by_amplification_success_plot(
  summary_samples_for_batch,
  amplification_threshold,
  sample_colours = default_sample_colours,
  reprep_threshold = 0.5,
  repool_threshold = 0.75,
  batch_name = ""
)

generate_reads_by_amplification_success_plots(
  summary_samples,
  read_threshold,
  sample_colours = default_sample_colours,
  reprep_threshold = 0.5,
  repool_threshold = 0.75
)
```

## Arguments

- summary_samples_for_batch, summary_samples:

  Output of
  [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md),
  for one batch or all batches.

- amplification_threshold, read_threshold:

  Reads needed to count a target as amplified, for the axis label.

- sample_colours:

  Named colours for each sample type.

- reprep_threshold, repool_threshold:

  QC thresholds drawn as dashed lines.

- batch_name:

  Batch name for the title.

## Value

A ggplot, or a named list of ggplots (one per batch) for the plural
function.
