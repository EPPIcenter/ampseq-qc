# ampseqQC

`ampseqQC` summarises QC statistics for targeted amplicon sequencing of Plasmodium processed with the [Mad4hatter](https://github.com/EPPIcenter/mad4hatter) pipeline. It combines the pipeline outputs with a sample manifest to produce an HTML QC report, pass/repool/reprep calls for each sample, control summaries and filtered allele tables.

Documentation: <https://eppicenter.github.io/ampseq-qc/>, including a [step-by-step tutorial](https://eppicenter.github.io/ampseq-qc/articles/ampseqQC.html) and an [example report](https://eppicenter.github.io/ampseq-qc/example-report/QC_report.html).

## Installation

```r
# install.packages("remotes")
remotes::install_github("EPPIcenter/ampseq-qc")
```

Rendering the report also requires the [Quarto CLI](https://quarto.org/docs/get-started/).

## Rendering the QC report

```r
library(ampseqQC)

render_qc_report(
  results_dir = "path/to/mad4hatter/results",
  manifest_file = "path/to/manifest.csv",
  output_dir = "qc_output"
)
```

Thresholds can be changed with the arguments of `render_qc_report()` (see `?render_qc_report`), for example `read_threshold`, `reprep_threshold` and `repool_threshold`.

From the command line:

```bash
Rscript -e 'ampseqQC::render_qc_report("path/to/results", "path/to/manifest.csv", output_dir = "qc_output")'
```

The report template can be found with `qc_report_template()` if you want to copy and customise it.

## Required inputs

The **results directory** from the Mad4hatter pipeline must include:

* **sample_coverage.txt**
* **amplicon_coverage.txt**
* **allele_data.txt**
* **panel_information/amplicon_info.tsv**

If present, **allele_data_collapsed.txt** and the **resistance_marker_module** tables are also filtered and written out.

A **sample manifest** (comma or semicolon separated) must contain:

* **sample_name** – Unique identifier for each sample.
* **SampleType** – Specifies whether the entry is a **sample**, **positive** control, or **negative** control.
* **Batch** – Identifies a group of samples processed simultaneously by the same individual.
* **Column** – The well column where the sample was placed in the plate.
* **Row** – The well row where the sample was placed in the plate.
* **Parasitemia** – The qPCR value for the sample.

## Panels, pools and reactions

The pools in a run are read from the `pool` column of `panel_information/amplicon_info.tsv`, so any combination or subset of pools is supported. Panel settings map each pool to the mPCR reaction it was amplified in, and list targets to exclude from QC calculations (they are still kept in the filtered allele tables).

`default_panel_settings()` covers the MAD4HatTeR and PfPHAST pools, including versioned and legacy names, using the recommended two-reaction layout:

| Reaction | Pools |
| -------- | ----- |
| 1 | D1, D1.1, 1A, R1, R1.1, R1.2, 1B, 5, M1, M1.1, M1.addon |
| 2 | R2, R2.1, 2, M2, M2.1 |

Targets shared by pools in different reactions are counted in each reaction. If a run contains a pool with no reaction defined, the report stops and asks for one. Use `panel_settings()` to add pools or change the layout:

```r
# Add a bespoke pool
panel <- panel_settings(c(AMPLseq = "3"), base = default_panel_settings())

# R1.2 run in its own reaction
panel <- panel_settings(c(R1 = "3", R1.2 = "3"), base = default_panel_settings())

render_qc_report("path/to/results", "path/to/manifest.csv", panel = panel)
```

## Using the functions directly

All of the QC steps are exported, so they can be used outside the report, for example:

```r
results <- read_mad4hatter_results("path/to/results")
manifest <- read_manifest("path/to/manifest.csv")

excluded <- qc_excluded_targets(results$panel_information)
panel_reactions <- assign_reactions(results$panel_information) |>
  dplyr::filter(!target_name %in% excluded$excluded)
amplicon_coverage_qc <- results$amplicon_coverage |>
  dplyr::filter(!target_name %in% excluded$excluded)

summary_samples <- summarise_samples(
  merge_amplicon_coverage(amplicon_coverage_qc, manifest, panel_reactions),
  count_targets_per_reaction(panel_reactions),
  manifest,
  read_threshold = 100
)
qc_calls <- generate_reprep_repool_table(summary_samples) |>
  fill_missing_data(manifest)
```

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```

## Acknowledgments

This code is based on QC plots developed by

* Andrés Aranda-Diaz
* Jessica Briggs
* Will Louie
