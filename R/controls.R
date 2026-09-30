#' Add within-sample allele frequency
#'
#' @param allele_data An allele table with `reads`.
#' @param group_cols Columns identifying a locus within a sample.
#' @return `allele_data` with an `AlleleFreq` column.
#' @export
add_allele_frequency <- function(allele_data, group_cols = c("sample_name", "target_name")) {
  allele_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(AlleleFreq = reads / sum(reads)) %>%
    dplyr::ungroup()
}

#' Summarise alleles per target in positive controls
#'
#' @param allele_data_qc Allele data with targets excluded from QC removed and
#'   `AlleleFreq` added (see [add_allele_frequency()]).
#' @param manifest Manifest from [read_manifest()].
#' @param allele_col Column used as the allele ID, `"asv"` or
#'   `"pseudocigar_masked"`. With `"pseudocigar_masked"` alleles are collapsed
#'   to unique masked pseudocigars.
#' @param read_filter Minimum reads for an allele to be counted.
#' @param af_filter Minimum within-sample allele frequency for an allele to be
#'   counted.
#'
#' @return A list with `locus_summary` (alleles per target per control),
#'   `category_counts` (targets per allele category per control) and
#'   `polyclonal_information` (alleles at multiallelic targets).
#' @export
summarise_positive_controls <- function(allele_data_qc, manifest, allele_col = "pseudocigar_masked",
                                        read_filter = 0, af_filter = 0.01) {
  pos_control_data <- allele_data_qc %>%
    dplyr::inner_join(manifest %>% dplyr::filter(SampleType == "positive"), by = "sample_name")

  if (allele_col == "pseudocigar_masked") {
    pos_control_data <- pos_control_data %>%
      dplyr::group_by(sample_name, target_name, pseudocigar_masked) %>%
      dplyr::summarise(reads = sum(reads), AlleleFreq = sum(AlleleFreq), .groups = "drop")
  }

  locus_summary <- pos_control_data %>%
    dplyr::group_by(sample_name, target_name) %>%
    dplyr::summarise(
      NumASVs_Meeting_Threshold = sum(reads > read_filter & AlleleFreq > af_filter),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Category = dplyr::case_when(
        NumASVs_Meeting_Threshold == 1 ~ "Monoallelic",
        NumASVs_Meeting_Threshold == 2 ~ "2 Alleles",
        NumASVs_Meeting_Threshold > 2 ~ ">2 Alleles"
      )
    )

  category_counts <- locus_summary %>%
    dplyr::group_by(sample_name, Category) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  polyclonal_information <- pos_control_data %>%
    dplyr::inner_join(locus_summary, by = c("sample_name", "target_name")) %>%
    dplyr::filter(Category %in% c("2 Alleles", ">2 Alleles"))

  list(
    locus_summary = locus_summary,
    category_counts = category_counts,
    polyclonal_information = polyclonal_information
  )
}

#' Negative control amplicon coverage
#'
#' @param amplicon_coverage_with_manifest Output of [merge_amplicon_coverage()].
#' @return Amplicon coverage for negative controls, with a `negative` label
#'   giving the batch and well.
#' @export
negative_control_amplicons <- function(amplicon_coverage_with_manifest) {
  amplicon_coverage_with_manifest %>%
    dplyr::filter(SampleType == "negative") %>%
    dplyr::mutate(negative = paste0(Batch, " Well: ", toupper(Row), Column))
}

#' Negative control targets above a read threshold
#'
#' @param amplicons_negative Output of [negative_control_amplicons()].
#' @param negative_control_read_threshold Reads above which a target is
#'   reported.
#' @return `sample_name`, `target_name` and `OutputPostprocessing` for targets
#'   above the threshold.
#' @export
negative_control_targets_over_threshold <- function(amplicons_negative, negative_control_read_threshold = 50) {
  amplicons_negative %>%
    dplyr::filter(OutputPostprocessing > negative_control_read_threshold) %>%
    dplyr::distinct(sample_name, target_name, OutputPostprocessing)
}

#' Total reads per target across negative controls
#'
#' Targets are ordered by chromosome and insert start from the panel
#' information.
#'
#' @param amplicons_negative Output of [negative_control_amplicons()].
#' @param panel_information Panel information from Mad4hatter.
#' @param nth_label Label every `nth_label` target on the x axis.
#'
#' @return One row per reaction and target with `sum_reads`,
#'   `samples_with_reads` (negative controls with reads), `Chromosome`, `Start`
#'   and `Locus_label`.
#' @export
summarise_negative_control_targets <- function(amplicons_negative, panel_information, nth_label = 20) {
  target_positions <- panel_information %>%
    dplyr::distinct(target_name, Chromosome = chrom, Start = insert_start)

  samples_with_reads <- amplicons_negative %>%
    dplyr::filter(reads > 0) %>%
    dplyr::group_by(target_name) %>%
    dplyr::summarise(samples_with_reads = paste(unique(sample_name), collapse = ", "), .groups = "drop")

  summary_data <- amplicons_negative %>%
    dplyr::group_by(reaction, target_name) %>%
    dplyr::summarise(sum_reads = sum(reads, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(target_positions, by = "target_name") %>%
    dplyr::left_join(samples_with_reads, by = "target_name")

  target_order <- summary_data %>%
    dplyr::distinct(target_name, Chromosome, Start) %>%
    dplyr::arrange(Chromosome, Start) %>%
    tibble::rowid_to_column("RowID") %>%
    dplyr::mutate(Locus_label = ifelse(RowID %% nth_label == 1, paste0(Chromosome, ":", Start), ""))

  summary_data %>%
    dplyr::inner_join(target_order %>% dplyr::select(target_name, RowID, Locus_label), by = "target_name") %>%
    dplyr::arrange(RowID) %>%
    dplyr::mutate(
      target_name = factor(target_name, levels = target_order$target_name),
      reaction = factor(reaction)
    )
}
