# Total reads per target across negative controls

Total reads per target across negative controls

## Usage

``` r
plot_negative_control_target_reads(
  summary_data,
  reaction_colours = default_reaction_colours
)
```

## Arguments

- summary_data:

  Output of
  [`summarise_negative_control_targets()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_negative_control_targets.md).

- reaction_colours:

  Colours for each reaction.

## Value

A ggplot with a `text` aesthetic for
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
tooltips.
