# Why manifest samples are missing from the allele data

Mad4hatter only writes a sample to the allele table when at least one
allele remains after DADA2 and post-processing, so samples with no final
reads (for example clean negative controls) are expected to be missing.

## Usage

``` r
samples_missing_from_allele_data(manifest, sample_coverage, allele_data)
```

## Arguments

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

- sample_coverage:

  Sample coverage from Mad4hatter.

- allele_data:

  Allele data from Mad4hatter.

## Value

Manifest samples (including controls) with no allele data, with
`reason`: either not in the pipeline results at all, or in the results
but with no alleles after DADA2 and post-processing.
