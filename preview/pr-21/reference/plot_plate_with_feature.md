# Plate heatmaps

Plate heatmaps

## Usage

``` r
plot_plate_with_feature(
  quadrants_batch,
  sample_colours = default_sample_colours,
  batch_name = "",
  fill_param,
  scale_midpoint,
  scale_label
)

plot_plate_heat_maps(
  summary_samples,
  quadrants,
  sample_colours = default_sample_colours,
  fill_param,
  scale_midpoint,
  scale_label
)
```

## Arguments

- quadrants_batch:

  Plate data for one batch joined to sample summaries.

- sample_colours:

  Named colours for each sample type, used for well outlines.

- batch_name:

  Batch name.

- fill_param:

  Expression, as a string, used for the fill, e.g. `"prop_good_loci"`.

- scale_midpoint:

  Midpoint of the fill scale.

- scale_label:

  Legend title.

- summary_samples:

  Output of
  [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md).

- quadrants:

  Output of
  [`create_plate_template()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/create_plate_template.md).

## Value

`plot_plate_with_feature()` returns a ggplot; `plot_plate_heat_maps()`
returns a named list of ggplots, one per batch.
