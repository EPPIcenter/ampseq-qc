# Add samples and reactions with no data

Samples in the manifest with no results, and samples missing a reaction,
are added with status `reprep`. Negative controls are not added.

## Usage

``` r
fill_missing_data(reprep_repool_summary, manifest)
```

## Arguments

- reprep_repool_summary:

  Output of
  [`generate_reprep_repool_table()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_reprep_repool_table.md).

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

## Value

`reprep_repool_summary` with the missing rows added.
