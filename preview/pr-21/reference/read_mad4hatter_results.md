# Read Mad4hatter results

Reads the outputs needed for QC from a Mad4hatter results directory.
`sample_coverage.txt`, `amplicon_coverage.txt`, `allele_data.txt` and
`panel_information/amplicon_info.tsv` are required. The collapsed allele
table and the resistance marker tables are read if present and are
`NULL` otherwise.

## Usage

``` r
read_mad4hatter_results(results_dir, standardise_sample_name = TRUE)
```

## Arguments

- results_dir:

  Path to the Mad4hatter results directory.

- standardise_sample_name:

  If `TRUE`, sample names are trimmed with
  [`standardise_sample_names()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/standardise_sample_names.md)
  to match the manifest.

## Value

A named list of data frames: `sample_coverage`, `amplicon_coverage`,
`allele_data`, `panel_information`, `collapsed_allele_data`,
`resmarker_table` and `resmarker_microhaplotype_table`.
