# Plate template

Builds one row per well for each batch, joined to the sample type in
that well.

## Usage

``` r
create_plate_template(summary_samples, batches, nrows = 8, ncols = 12)
```

## Arguments

- summary_samples:

  Output of
  [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md).

- batches:

  Batches to include.

- nrows, ncols:

  Plate dimensions.

## Value

A data frame of wells with tile coordinates.
