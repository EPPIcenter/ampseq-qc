# Print plots as a Quarto tabset

Must be called from a chunk with `results: asis`.

## Usage

``` r
create_tabset(plots)
```

## Arguments

- plots:

  Named list of ggplots; names are used as tab titles.

## Value

`NULL`, invisibly.
