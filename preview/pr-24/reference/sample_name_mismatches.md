# Compare manifest and pipeline sample names

The amplicon coverage contains every sample that went through
Mad4hatter, so it is used to check that the manifest and pipeline sample
names match. `check_sample_names()` stops with an error if no names
match and warns if some samples are only in the manifest or only in the
pipeline outputs.

## Usage

``` r
sample_name_mismatches(manifest, amplicon_coverage)

check_sample_names(manifest, amplicon_coverage)
```

## Arguments

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-24/reference/read_manifest.md).

- amplicon_coverage:

  Amplicon coverage from
  [`read_mad4hatter_results()`](https://eppicenter.github.io/ampseq-qc/preview/pr-24/reference/read_mad4hatter_results.md).

## Value

A list with `not_in_results` (manifest samples not in the amplicon
coverage), `not_in_manifest` (amplicon coverage samples not in the
manifest) and `n_matched`. `check_sample_names()` returns it invisibly.
