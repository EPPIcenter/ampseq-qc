# Summarise amplification success per sample and reaction

A target is considered successfully amplified if it has more than
`read_threshold` reads after postprocessing.

## Usage

``` r
summarise_samples(
  amplicon_coverage_with_manifest,
  nloci_table,
  manifest,
  read_threshold = 100
)
```

## Arguments

- amplicon_coverage_with_manifest:

  Output of
  [`merge_amplicon_coverage()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/merge_amplicon_coverage.md).

- nloci_table:

  Output of
  [`count_targets_per_reaction()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/count_targets_per_reaction.md).

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

- read_threshold:

  Reads needed to count a target as amplified.

## Value

One row per sample and reaction with `reads_per_reaction`,
`n_good_loci`, `reads_per_sample` and `prop_good_loci`, plus the
manifest columns.
