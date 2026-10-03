# Total reads per target across negative controls

Sums the final reads (`OutputPostprocessing`) for each target across all
negative controls, the same reads used by
[`plot_negative_control_histogram()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_negative_control_histogram.md)
and
[`negative_control_targets_over_threshold()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_targets_over_threshold.md).
Targets are ordered by chromosome and insert start from the panel
information.

## Usage

``` r
summarise_negative_control_targets(
  amplicons_negative,
  panel_information,
  nth_label = 20
)
```

## Arguments

- amplicons_negative:

  Output of
  [`negative_control_amplicons()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_amplicons.md).

- panel_information:

  Panel information from Mad4hatter.

- nth_label:

  Label every `nth_label` target on the x axis.

## Value

One row per reaction and target with `sum_reads`, `samples_with_reads`
(negative controls with reads), `Chromosome`, `Start` and `Locus_label`.
