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
