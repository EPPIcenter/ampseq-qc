#' Standardise sample names
#'
#' Trims the sequencing information Mad4hatter keeps in sample names (a
#' trailing `_L001` and the final `_`-separated field, e.g. `_S12`) so that
#' they match the manifest.
#'
#' @param sample_names Character vector of sample names.
#' @return Character vector of standardised sample names.
#' @export
#' @examples
#' standardise_sample_names(c("sample1_S1_L001", "sample2_S2"))
standardise_sample_names <- function(sample_names) {
  sample_names <- sub("_L001$", "", sample_names)
  stringr::word(sample_names, start = 1, end = -2, sep = "_")
}

#' Read a sample manifest
#'
#' The manifest must contain `sample_name`, `SampleType` (`sample`,
#' `positive` or `negative`), `Batch`, `Column`, `Row` and `Parasitemia`.
#' Comma and semicolon separated files are supported.
#'
#' @param manifest_file Path to the manifest.
#' @return The manifest as a data frame, with `Row` in upper case.
#' @export
read_manifest <- function(manifest_file) {
  if (!file.exists(manifest_file)) {
    stop("Manifest not found: ", manifest_file, call. = FALSE)
  }
  sep_guess <- if (any(grepl(";", readLines(manifest_file, n = 1)))) ";" else ","
  manifest <- utils::read.csv(manifest_file, sep = sep_guess)

  required_columns <- c("sample_name", "SampleType", "Batch", "Column", "Row", "Parasitemia")
  missing_columns <- setdiff(required_columns, names(manifest))
  if (length(missing_columns) > 0) {
    stop("Manifest is missing required column(s): ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  manifest %>%
    dplyr::mutate(Row = toupper(Row))
}

#' Read Mad4hatter results
#'
#' Reads the outputs needed for QC from a Mad4hatter results directory.
#' `sample_coverage.txt`, `amplicon_coverage.txt`, `allele_data.txt` and
#' `panel_information/amplicon_info.tsv` are required. The collapsed allele
#' table and the resistance marker tables are read if present and are `NULL`
#' otherwise.
#'
#' @param results_dir Path to the Mad4hatter results directory.
#' @param standardise_sample_name If `TRUE`, sample names are trimmed with
#'   [standardise_sample_names()] to match the manifest.
#'
#' @return A named list of data frames: `sample_coverage`,
#'   `amplicon_coverage`, `allele_data`, `panel_information`,
#'   `collapsed_allele_data`, `resmarker_table` and
#'   `resmarker_microhaplotype_table`.
#' @export
read_mad4hatter_results <- function(results_dir, standardise_sample_name = TRUE) {
  required_files <- c(
    sample_coverage = "sample_coverage.txt",
    amplicon_coverage = "amplicon_coverage.txt",
    allele_data = "allele_data.txt",
    panel_information = file.path("panel_information", "amplicon_info.tsv")
  )
  optional_files <- c(
    collapsed_allele_data = "allele_data_collapsed.txt",
    resmarker_table = file.path("resistance_marker_module", "resmarker_table.txt"),
    resmarker_microhaplotype_table = file.path("resistance_marker_module", "resmarker_microhaplotype_table.txt")
  )

  required_paths <- file.path(results_dir, required_files)
  missing_files <- required_files[!file.exists(required_paths)]
  if (length(missing_files) > 0) {
    stop("Required Mad4hatter output(s) not found in ", results_dir, ": ", paste(missing_files, collapse = ", "), call. = FALSE)
  }

  results <- lapply(required_paths, utils::read.delim)
  names(results) <- names(required_files)

  for (name in names(optional_files)) {
    path <- file.path(results_dir, optional_files[[name]])
    results[name] <- list(if (file.exists(path)) utils::read.delim(path) else NULL)
  }

  if (standardise_sample_name) {
    for (name in c("sample_coverage", "amplicon_coverage", "allele_data", "collapsed_allele_data")) {
      if (!is.null(results[[name]])) {
        results[[name]]$sample_name <- standardise_sample_names(results[[name]]$sample_name)
      }
    }
  }

  results
}
