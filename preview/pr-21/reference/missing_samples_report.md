# Samples in the manifest missing from the allele data

Samples in the manifest missing from the allele data

## Usage

``` r
missing_samples_report(allele_data, manifest)
```

## Arguments

- allele_data:

  Allele data from Mad4hatter.

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

## Value

Manifest rows (excluding negative controls) with no allele data.
