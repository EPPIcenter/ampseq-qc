#' Filter an allele table
#'
#' Keeps alleles with more than `read_filter` reads and, if `filter_wsaf` is
#' `TRUE`, a within-sample allele frequency above `af_filter`.
#'
#' @param allele_table Allele or resistance marker table with `reads`.
#' @param group_cols Columns identifying a locus within a sample, used to
#'   calculate allele frequency.
#' @param read_filter Minimum reads per allele.
#' @param af_filter Minimum within-sample allele frequency.
#' @param filter_wsaf If `FALSE`, only `read_filter` is applied.
#'
#' @return The filtered table, with the same columns as `allele_table`.
#' @export
filter_allele_table <- function(allele_table, group_cols = c("sample_name", "target_name"),
                                read_filter = 0, af_filter = 0.01, filter_wsaf = TRUE) {
  filtered <- add_allele_frequency(allele_table, group_cols)
  filtered <- if (filter_wsaf) {
    dplyr::filter(filtered, reads > read_filter & AlleleFreq > af_filter)
  } else {
    dplyr::filter(filtered, reads > read_filter)
  }
  dplyr::select(filtered, -AlleleFreq)
}

#' Locus columns for Mad4hatter output tables
#'
#' @return A named list of the columns identifying a locus within a sample for
#'   each allele table Mad4hatter produces.
#' @export
allele_table_group_cols <- function() {
  list(
    allele_data = c("sample_name", "target_name"),
    collapsed_allele_data = c("sample_name", "target_name"),
    resmarker_table = c("sample_name", "gene_id", "gene", "aa_position"),
    resmarker_microhaplotype_table = c("sample_name", "gene_id", "gene", "target_name", "mhap_aa_positions", "ref_mhap")
  )
}

#' Number of samples in each input file
#'
#' @param manifest Manifest from [read_manifest()].
#' @param tables Named list of tables with a `sample_name` column, for example
#'   the pipeline outputs from [read_mad4hatter_results()]. Names are used as
#'   the file labels.
#' @return One row per table with `file`, `samples` (distinct sample names in
#'   the table) and `in_manifest` (how many of those are in the manifest),
#'   starting with the manifest itself.
#' @export
sample_counts_by_file <- function(manifest, tables) {
  manifest_samples <- unique(manifest$sample_name)
  file_samples <- c(list(Manifest = manifest_samples), lapply(tables, function(table) unique(table$sample_name)))

  tibble::tibble(
    file = names(file_samples),
    samples = vapply(file_samples, length, integer(1), USE.NAMES = FALSE),
    in_manifest = vapply(file_samples, function(s) sum(s %in% manifest_samples), integer(1), USE.NAMES = FALSE)
  )
}

#' Why manifest samples are missing from the allele data
#'
#' Mad4hatter only writes a sample to the allele table when at least one allele
#' remains after DADA2 and post-processing, so samples with no final reads
#' (for example clean negative controls) are expected to be missing.
#'
#' @param manifest Manifest from [read_manifest()].
#' @param sample_coverage Sample coverage from Mad4hatter.
#' @param allele_data Allele data from Mad4hatter.
#' @return Manifest samples (including controls) with no allele data, with
#'   `reason`: either not in the pipeline results at all, or in the results but
#'   with no alleles after DADA2 and post-processing.
#' @export
samples_missing_from_allele_data <- function(manifest, sample_coverage, allele_data) {
  manifest %>%
    dplyr::filter(!(sample_name %in% allele_data$sample_name)) %>%
    dplyr::mutate(reason = dplyr::if_else(
      sample_name %in% sample_coverage$sample_name,
      "No alleles after DADA2 and post-processing",
      "Not in pipeline results (not sequenced, or name does not match the manifest)"
    )) %>%
    dplyr::select(sample_name, Batch, SampleType, reason) %>%
    dplyr::arrange(SampleType, Batch, sample_name)
}

#' Samples in the manifest missing from the allele data
#'
#' @param allele_data Allele data from Mad4hatter.
#' @param manifest Manifest from [read_manifest()].
#' @return Manifest rows (excluding negative controls) with no allele data.
#' @export
missing_samples_report <- function(allele_data, manifest) {
  manifest %>%
    dplyr::filter(!(sample_name %in% allele_data$sample_name)) %>%
    dplyr::filter(SampleType != "negative") %>%
    dplyr::select(sample_name, Batch, SampleType, Row, Column, Parasitemia) %>%
    dplyr::arrange(Batch, sample_name)
}

#' Targets in the panel missing from the allele data
#'
#' @param panel_information Panel information from Mad4hatter.
#' @param allele_data Allele data from Mad4hatter.
#' @return `target_name`, `pool` and `chrom` of targets with no allele data.
#' @export
missing_targets_report <- function(panel_information, allele_data) {
  panel_information %>%
    dplyr::anti_join(allele_data, by = "target_name") %>%
    dplyr::select(target_name, pool, chrom)
}
