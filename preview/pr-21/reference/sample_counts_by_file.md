# Number of samples in each input file

Number of samples in each input file

## Usage

``` r
sample_counts_by_file(manifest, tables)
```

## Arguments

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

- tables:

  Named list of tables with a `sample_name` column, for example the
  pipeline outputs from
  [`read_mad4hatter_results()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_mad4hatter_results.md).
  Names are used as the file labels.

## Value

One row per table with `file`, `samples` (distinct sample names in the
table) and `in_manifest` (how many of those are in the manifest),
starting with the manifest itself.
