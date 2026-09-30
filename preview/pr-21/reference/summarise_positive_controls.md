# Summarise alleles per target in positive controls

Summarise alleles per target in positive controls

## Usage

``` r
summarise_positive_controls(
  allele_data_qc,
  manifest,
  allele_col = "pseudocigar_masked",
  read_filter = 0,
  af_filter = 0.01
)
```

## Arguments

- allele_data_qc:

  Allele data with targets excluded from QC removed and `AlleleFreq`
  added (see
  [`add_allele_frequency()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/add_allele_frequency.md)).

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

- allele_col:

  Column used as the allele ID, `"asv"` or `"pseudocigar_masked"`. With
  `"pseudocigar_masked"` alleles are collapsed to unique masked
  pseudocigars.

- read_filter:

  Minimum reads for an allele to be counted.

- af_filter:

  Minimum within-sample allele frequency for an allele to be counted.

## Value

A list with `locus_summary` (alleles per target per control),
`category_counts` (targets per allele category per control) and
`polyclonal_information` (alleles at multiallelic targets).
