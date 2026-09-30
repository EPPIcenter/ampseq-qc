# Plate layout plots

Plate layout plots

## Usage

``` r
plot_plate(
  quadrants_batch,
  sample_colours = default_sample_colours,
  batch_name = ""
)

plot_plate_layouts(plate_data, sample_colours = default_sample_colours)
```

## Arguments

- quadrants_batch:

  Plate template for one batch.

- sample_colours:

  Named colours for each sample type.

- batch_name:

  Batch name for the title.

- plate_data:

  Output of
  [`create_plate_template()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/create_plate_template.md).

## Value

`plot_plate()` returns a ggplot; `plot_plate_layouts()` returns a named
list of ggplots, one per batch.
