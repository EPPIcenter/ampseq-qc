# Add the manifest to sample coverage

Add the manifest to sample coverage

## Usage

``` r
merge_sample_coverage(sample_coverage, manifest)
```

## Arguments

- sample_coverage:

  Sample coverage from Mad4hatter.

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

## Value

One row per sample with a column per pipeline stage, joined to the
manifest.
