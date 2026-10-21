# Print a plot at a given size

Knits the plot as a child chunk so its size can differ from the calling
chunk. Must be called from a chunk with `results: asis`.

## Usage

``` r
print_sized_plot(plot, fig_width, fig_height)
```

## Arguments

- plot:

  A ggplot.

- fig_width, fig_height:

  Figure size in inches.

## Value

`NULL`, invisibly.
