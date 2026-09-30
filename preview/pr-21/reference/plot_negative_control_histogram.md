# Read distribution per negative control

Read distribution per negative control

## Usage

``` r
plot_negative_control_histogram(
  data,
  reaction_colours = default_reaction_colours,
  batch_name = NULL,
  remove_zeros = TRUE,
  ncol_layout = 3
)
```

## Arguments

- data:

  Negative control amplicon coverage, from
  [`negative_control_amplicons()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_amplicons.md).

- reaction_colours:

  Colours for each reaction.

- batch_name:

  Optional batch name for the title.

- remove_zeros:

  If `TRUE`, targets with no reads are not plotted.

- ncol_layout:

  Number of facet columns.

## Value

A ggplot, or `NULL` if there is nothing to plot.
