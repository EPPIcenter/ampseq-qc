#' Targets excluded from QC
#'
#' Combines the targets excluded in the panel settings with long targets
#' (insert plus primers longer than `long_target_threshold`), which are known
#' to perform less well.
#'
#' @param panel_information Data frame read from
#'   `panel_information/amplicon_info.tsv`.
#' @param settings Panel settings, see [panel_settings()].
#' @param long_target_threshold Length in bp, including primers, above which a
#'   target is considered long.
#'
#' @return A list with `excluded` (all excluded target names) and `long` (the
#'   long target names).
#' @export
qc_excluded_targets <- function(panel_information, settings = default_panel_settings(), long_target_threshold = 275) {
  long_targets <- panel_information %>%
    dplyr::mutate(target_length = (insert_end - insert_start) + nchar(fwd_primer) + nchar(rev_primer)) %>%
    dplyr::filter(target_length > long_target_threshold) %>%
    dplyr::pull(target_name) %>%
    unique()

  list(
    excluded = unique(c(settings$excluded_targets, long_targets)),
    long = long_targets
  )
}

#' Count targets per reaction
#'
#' @param panel_reactions Panel information with reactions, from
#'   [assign_reactions()].
#' @return A data frame with `reaction` and `nreactionloci`.
#' @export
count_targets_per_reaction <- function(panel_reactions) {
  panel_reactions %>%
    dplyr::distinct(target_name, reaction) %>%
    dplyr::count(reaction) %>%
    dplyr::rename(nreactionloci = n)
}

#' Add the manifest to sample coverage
#'
#' @param sample_coverage Sample coverage from Mad4hatter.
#' @param manifest Manifest from [read_manifest()].
#' @return One row per sample with a column per pipeline stage, joined to the
#'   manifest.
#' @export
merge_sample_coverage <- function(sample_coverage, manifest) {
  sample_coverage %>%
    dplyr::mutate(reads = suppressWarnings(as.numeric(reads))) %>%
    tidyr::replace_na(list(reads = 0)) %>%
    tidyr::pivot_wider(names_from = stage, values_from = reads) %>%
    dplyr::inner_join(manifest, by = "sample_name")
}

#' Add the manifest and reactions to amplicon coverage
#'
#' @param amplicon_coverage Amplicon coverage from Mad4hatter.
#' @param manifest Manifest from [read_manifest()].
#' @param panel_reactions Panel information with reactions, from
#'   [assign_reactions()].
#' @return Amplicon coverage with manifest columns and a `reaction` column.
#'   Targets in more than one reaction appear once per reaction.
#' @export
merge_amplicon_coverage <- function(amplicon_coverage, manifest, panel_reactions) {
  amplicon_coverage %>%
    dplyr::inner_join(manifest, by = "sample_name") %>%
    dplyr::left_join(
      panel_reactions %>% dplyr::distinct(target_name, reaction),
      by = "target_name",
      relationship = "many-to-many"
    )
}

#' Summarise amplification success per sample and reaction
#'
#' A target is considered successfully amplified if it has more than
#' `read_threshold` reads after postprocessing.
#'
#' @param amplicon_coverage_with_manifest Output of [merge_amplicon_coverage()].
#' @param nloci_table Output of [count_targets_per_reaction()].
#' @param manifest Manifest from [read_manifest()].
#' @param read_threshold Reads needed to count a target as amplified.
#'
#' @return One row per sample and reaction with `reads_per_reaction`,
#'   `n_good_loci`, `reads_per_sample` and `prop_good_loci`, plus the manifest
#'   columns.
#' @export
summarise_samples <- function(amplicon_coverage_with_manifest, nloci_table, manifest, read_threshold = 100) {
  reads_per_sample <- amplicon_coverage_with_manifest %>%
    dplyr::distinct(sample_name, target_name, .keep_all = TRUE) %>%
    dplyr::group_by(sample_name) %>%
    dplyr::summarise(reads_per_sample = sum(OutputPostprocessing), .groups = "drop")

  amplicon_coverage_with_manifest %>%
    dplyr::left_join(nloci_table, by = "reaction") %>%
    dplyr::group_by(sample_name, Batch, SampleType, reaction, nreactionloci) %>%
    dplyr::summarise(
      reads_per_reaction = sum(OutputPostprocessing),
      n_good_loci = sum(OutputPostprocessing > read_threshold),
      .groups = "drop"
    ) %>%
    dplyr::left_join(reads_per_sample, by = "sample_name") %>%
    dplyr::mutate(prop_good_loci = n_good_loci / nreactionloci) %>%
    dplyr::inner_join(manifest, by = dplyr::join_by(sample_name, Batch, SampleType))
}

#' Classify samples as pass, repool or reprep
#'
#' Negative controls are not classified.
#'
#' @param summary_samples Output of [summarise_samples()].
#' @param reprep_threshold Proportion of targets amplified below which a
#'   sample needs re-prep.
#' @param repool_threshold Proportion of targets amplified below which a
#'   sample needs re-pool.
#' @return One row per sample and reaction with `status` and `reason`.
#' @export
generate_reprep_repool_table <- function(summary_samples, reprep_threshold = 0.5, repool_threshold = 0.75) {
  summary_samples %>%
    dplyr::filter(SampleType != "negative") %>%
    dplyr::mutate(
      status = dplyr::case_when(
        prop_good_loci < reprep_threshold ~ "reprep",
        prop_good_loci < repool_threshold ~ "repool",
        TRUE ~ "pass"
      ),
      reason = dplyr::case_when(
        prop_good_loci < reprep_threshold ~ paste("< reprep threshold ", reprep_threshold),
        prop_good_loci < repool_threshold ~ paste("< repool threshold ", repool_threshold),
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(status)) %>%
    dplyr::select(sample_name, Batch, SampleType, reaction, status, reason, reads_per_reaction, prop_good_loci)
}

#' Add samples and reactions with no data
#'
#' Samples in the manifest with no results, and samples missing a reaction,
#' are added with status `reprep`. Negative controls are not added.
#'
#' @param reprep_repool_summary Output of [generate_reprep_repool_table()].
#' @param manifest Manifest from [read_manifest()].
#' @return `reprep_repool_summary` with the missing rows added.
#' @export
fill_missing_data <- function(reprep_repool_summary, manifest) {
  unique_reactions <- unique(reprep_repool_summary$reaction)

  missing_samples <- manifest %>%
    dplyr::filter(SampleType != "negative") %>%
    dplyr::filter(!(sample_name %in% reprep_repool_summary$sample_name)) %>%
    dplyr::select(sample_name, Batch, SampleType)

  missing_sample_rows <- missing_samples %>%
    tidyr::crossing(reaction = unique_reactions) %>%
    dplyr::mutate(
      status = "reprep",
      reason = "no data for sample",
      reads_per_reaction = 0,
      prop_good_loci = 0
    )

  missing_reaction_rows <- reprep_repool_summary %>%
    dplyr::select(sample_name, Batch, SampleType) %>%
    dplyr::distinct() %>%
    tidyr::crossing(reaction = unique_reactions) %>%
    dplyr::anti_join(reprep_repool_summary, by = c("sample_name", "Batch", "reaction")) %>%
    dplyr::mutate(
      status = "reprep",
      reason = "no data for reaction",
      reads_per_reaction = 0,
      prop_good_loci = 0
    )

  dplyr::bind_rows(reprep_repool_summary, missing_sample_rows, missing_reaction_rows)
}

is_control <- function(sample_type) {
  sample_type %in% c("positive", "negative")
}

#' Samples with a given QC status
#'
#' @param reprep_repool_summary Output of [fill_missing_data()].
#' @param qc_status `"repool"` or `"reprep"`.
#' @return The rows with that status, sorted by batch, sample and reaction.
#' @export
samples_with_status <- function(reprep_repool_summary, qc_status) {
  reprep_repool_summary %>%
    dplyr::filter(status == qc_status) %>%
    dplyr::select(sample_name, Batch, reaction, reads_per_reaction, prop_good_loci, reason) %>%
    dplyr::arrange(Batch, sample_name, reaction)
}

#' Sample-level QC status
#'
#' Combines the per-reaction calls into one status per sample, taking the worst
#' status across reactions (`reprep`, then `repool`, then `pass`). Positive and
#' negative controls are excluded.
#'
#' @param reprep_repool_summary Output of [fill_missing_data()].
#' @return One row per sample with `sample_name`, `Batch` and `status`.
#' @export
sample_qc_status <- function(reprep_repool_summary) {
  status_levels <- c("pass", "repool", "reprep")

  reprep_repool_summary %>%
    dplyr::filter(!is_control(SampleType)) %>%
    dplyr::mutate(severity = match(status, status_levels)) %>%
    dplyr::group_by(sample_name, Batch) %>%
    dplyr::summarise(status = status_levels[max(severity)], .groups = "drop")
}

#' Overall QC counts
#'
#' Counts samples (excluding positive and negative controls) by their
#' sample-level status from [sample_qc_status()], so each sample is counted
#' once.
#'
#' @param reprep_repool_summary Output of [fill_missing_data()].
#' @return A list with `reprep`, `repool`, `pass`, `total` and
#'   `percentage_pass` (`NA` if there are no samples).
#' @export
qc_status_counts <- function(reprep_repool_summary) {
  samples <- sample_qc_status(reprep_repool_summary)

  counts <- list(
    reprep = sum(samples$status == "reprep"),
    repool = sum(samples$status == "repool"),
    pass = sum(samples$status == "pass")
  )
  counts$total <- nrow(samples)
  counts$percentage_pass <- if (counts$total > 0) counts$pass / counts$total * 100 else NA_real_
  counts
}

#' QC status by batch
#'
#' Samples are counted once, by their sample-level status from
#' [sample_qc_status()].
#'
#' @param reprep_repool_summary Output of [fill_missing_data()].
#' @return `qc_summary_by_batch()` returns counts and percentages per batch and
#'   status (excluding controls). `qc_summary_table()` returns one row per batch
#'   with pass, repool and reprep counts and the pass rate.
#' @export
qc_summary_by_batch <- function(reprep_repool_summary) {
  sample_qc_status(reprep_repool_summary) %>%
    dplyr::count(Batch, status, name = "count") %>%
    dplyr::group_by(Batch) %>%
    dplyr::mutate(
      total = sum(count),
      percentage = (count / total) * 100
    ) %>%
    dplyr::ungroup()
}

#' @rdname qc_summary_by_batch
#' @export
qc_summary_table <- function(reprep_repool_summary) {
  sample_qc_status(reprep_repool_summary) %>%
    dplyr::count(Batch, status, name = "count") %>%
    tidyr::complete(Batch, status = c("pass", "repool", "reprep"), fill = list(count = 0)) %>%
    tidyr::pivot_wider(names_from = status, values_from = count, values_fill = 0) %>%
    dplyr::mutate(
      total = pass + repool + reprep,
      pass_rate = round(100 * pass / total, 1)
    ) %>%
    dplyr::select(Batch, pass, repool, reprep, total, pass_rate)
}
