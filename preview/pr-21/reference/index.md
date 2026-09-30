# Package index

## QC report

- [`render_qc_report()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/render_qc_report.md)
  : Render the QC report
- [`qc_report_template()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/qc_report_template.md)
  : Path to the QC report template

## Loading data

- [`read_mad4hatter_results()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_mad4hatter_results.md)
  : Read Mad4hatter results
- [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md)
  : Read a sample manifest
- [`standardise_sample_names()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/standardise_sample_names.md)
  : Standardise sample names

## Panel settings

Map pools to reactions and choose targets to exclude from QC.

- [`panel_settings()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/panel_settings.md)
  : Panel settings
- [`default_panel_settings()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/default_panel_settings.md)
  : Default panel settings for MAD4HatTeR and PfPHAST
- [`assign_reactions()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/assign_reactions.md)
  : Assign targets to reactions
- [`summarise_pools()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_pools.md)
  : Summarise pools in a run
- [`qc_excluded_targets()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/qc_excluded_targets.md)
  : Targets excluded from QC

## Amplification success and QC calls

- [`merge_sample_coverage()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/merge_sample_coverage.md)
  : Add the manifest to sample coverage
- [`merge_amplicon_coverage()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/merge_amplicon_coverage.md)
  : Add the manifest and reactions to amplicon coverage
- [`count_targets_per_reaction()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/count_targets_per_reaction.md)
  : Count targets per reaction
- [`summarise_samples()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_samples.md)
  : Summarise amplification success per sample and reaction
- [`generate_reprep_repool_table()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_reprep_repool_table.md)
  : Classify samples as pass, repool or reprep
- [`fill_missing_data()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/fill_missing_data.md)
  : Add samples and reactions with no data
- [`samples_with_status()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/samples_with_status.md)
  : Samples with a given QC status
- [`qc_status_counts()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/qc_status_counts.md)
  : Overall QC counts
- [`qc_summary_by_batch()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/qc_summary_by_batch.md)
  [`qc_summary_table()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/qc_summary_by_batch.md)
  : QC status by batch

## Controls

- [`add_allele_frequency()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/add_allele_frequency.md)
  : Add within-sample allele frequency
- [`summarise_positive_controls()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_positive_controls.md)
  : Summarise alleles per target in positive controls
- [`negative_control_amplicons()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_amplicons.md)
  : Negative control amplicon coverage
- [`negative_control_targets_over_threshold()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_targets_over_threshold.md)
  : Negative control targets above a read threshold
- [`summarise_negative_control_targets()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/summarise_negative_control_targets.md)
  : Total reads per target across negative controls

## Output tables

- [`filter_allele_table()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/filter_allele_table.md)
  : Filter an allele table
- [`allele_table_group_cols()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/allele_table_group_cols.md)
  : Locus columns for Mad4hatter output tables
- [`missing_samples_report()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/missing_samples_report.md)
  : Samples in the manifest missing from the allele data
- [`missing_targets_report()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/missing_targets_report.md)
  : Targets in the panel missing from the allele data

## Plots

- [`create_plate_template()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/create_plate_template.md)
  : Plate template
- [`plot_plate()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_plate.md)
  [`plot_plate_layouts()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_plate.md)
  : Plate layout plots
- [`generate_dimer_plot()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_dimer_plot.md)
  : Primer dimer content
- [`generate_balancing_plot()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_balancing_plot.md)
  : Balancing across batches
- [`generate_reads_by_amplification_success_plot()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_reads_by_amplification_success_plot.md)
  [`generate_reads_by_amplification_success_plots()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_reads_by_amplification_success_plot.md)
  : Sample reads against amplification success
- [`generate_parasitemia_by_amplification_success_plot()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_parasitemia_by_amplification_success_plot.md)
  [`generate_parasitemia_by_amplification_success_plots()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/generate_parasitemia_by_amplification_success_plot.md)
  : Parasitemia against amplification success
- [`plot_plate_with_feature()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_plate_with_feature.md)
  [`plot_plate_heat_maps()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_plate_with_feature.md)
  : Plate heatmaps
- [`plot_positive_control_alleles()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_positive_control_alleles.md)
  : Alleles per target in positive controls
- [`plot_negative_control_histogram()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_negative_control_histogram.md)
  : Read distribution per negative control
- [`negative_control_plot_height()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/negative_control_plot_height.md)
  : Figure height for negative control histograms
- [`plot_negative_control_target_reads()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_negative_control_target_reads.md)
  : Total reads per target across negative controls
- [`plot_qc_status_by_batch()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/plot_qc_status_by_batch.md)
  : QC status by batch

## Report helpers

Helpers used by the report template.

- [`styled_table()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/styled_table.md)
  : Styled HTML table
- [`interactive_plot()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/interactive_plot.md)
  : Interactive version of a ggplot
- [`create_tabset()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/create_tabset.md)
  : Print plots as a Quarto tabset
- [`print_sized_plot()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/print_sized_plot.md)
  : Print a plot at a given size
