# Render the QC report

Renders the QC report to HTML with the Quarto CLI and writes the QC
output tables (repool/reprep summaries, control summaries, filtered
allele tables and missing sample/target reports) to `output_dir`.

## Usage

``` r
render_qc_report(
  results_dir,
  manifest_file,
  output_dir = ".",
  output_file = "QC_report.html",
  panel = default_panel_settings(),
  standardise_sample_name = TRUE,
  read_threshold = 100,
  read_filter = 0,
  af_filter = 0.01,
  negative_control_read_threshold = 50,
  reprep_threshold = 0.5,
  repool_threshold = 0.75,
  allele_col = "pseudocigar_masked",
  long_target_threshold = 275,
  filter_wsaf_final_allele_table = TRUE,
  quarto = Sys.which("quarto")
)
```

## Arguments

- results_dir:

  Mad4hatter results directory.

- manifest_file:

  Sample manifest, see
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

- output_dir:

  Directory for the report and output tables.

- output_file:

  File name of the HTML report.

- panel:

  Panel settings, see
  [`panel_settings()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/panel_settings.md).

- standardise_sample_name:

  If `TRUE`, sample names are trimmed to match the manifest, see
  [`standardise_sample_names()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/standardise_sample_names.md).

- read_threshold:

  Reads needed to count a target as successfully amplified.

- read_filter:

  Minimum reads per allele, for the positive control plot and the
  filtered allele tables.

- af_filter:

  Minimum within-sample allele frequency, for the positive control plot
  and (if `filter_wsaf_final_allele_table`) the filtered allele tables.

- negative_control_read_threshold:

  Reads above which a negative control target is reported.

- reprep_threshold:

  Proportion of targets amplified below which a sample needs re-prep.

- repool_threshold:

  Proportion of targets amplified below which a sample needs re-pool.

- allele_col:

  Allele ID column, `"asv"` or `"pseudocigar_masked"`.

- long_target_threshold:

  Targets longer than this (bp, including primers) are excluded from QC.

- filter_wsaf_final_allele_table:

  If `FALSE`, only `read_filter` is applied to the filtered allele
  tables.

- quarto:

  Path to the Quarto CLI.

## Value

Path to the rendered report, invisibly.
