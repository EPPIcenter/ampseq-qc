# Parasitemia against amplification success

Parasitemia against amplification success

## Usage

``` r
generate_parasitemia_by_amplification_success_plot(
  summary_samples_for_batch,
  threshold,
  sample_colours = default_sample_colours,
  batch_name = ""
)

generate_parasitemia_by_amplification_success_plots(
  summary_samples,
  threshold,
  sample_colours = default_sample_colours
)
```

## Arguments

- summary_samples_for_batch, summary_samples:

  Output of
  [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md),
  for one batch or all batches.

- threshold:

  Reads needed to count a target as amplified, for the axis label.

- sample_colours:

  Named colours for each sample type.

- batch_name:

  Batch name for the title.

## Value

A ggplot, or a named list of ggplots (one per batch) for the plural
function.
