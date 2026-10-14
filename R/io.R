#' Remove Illumina FASTQ suffixes from sample names
#'
#' Sample names in Mad4hatter outputs are the FASTQ file names up to the read
#' (`_R1`/`_R2`), so they can still carry information added by the sequencer.
#' This removes only those parts so the names match the manifest: the sample
#' number (`_S12`) and, if present, the lane that follows it (`_L001`). A lane
#' without a sample number is not removed, as Illumina always adds both.
#' Anything else in the name is kept.
#'
#' A name that ends in `_S` followed by digits but has no sequencer suffix
#' (for example `B1_S01`) would lose that ending, so names listed in `keep`,
#' usually the manifest sample names, are returned unchanged.
#'
#' @param sample_names Character vector of sample names.
#' @param keep Names to return unchanged.
#' @return Character vector of standardised sample names.
#' @export
#' @examples
#' standardise_illumina_sample_name(c("sample1_S1_L001", "sample_A_S2"))
#'
#' # Without `keep`, a name ending in _S<number> is trimmed
#' standardise_illumina_sample_name("B1_S01")
#' standardise_illumina_sample_name("B1_S01", keep = "B1_S01")
standardise_illumina_sample_name <- function(sample_names, keep = character()) {
  trimmed <- sub("_S[0-9]+(_L[0-9]{3})?$", "", sample_names)
  ifelse(sample_names %in% keep, sample_names, trimmed)
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
#' @param standardise_sample_name If `TRUE`, Illumina suffixes are removed
#'   from sample names with [standardise_illumina_sample_name()] so they match
#'   the manifest. A warning is given if this makes different sample names
#'   identical.
#' @param manifest_sample_names Sample names from the manifest. Names that
#'   already match exactly are not trimmed.
#'
#' @return A named list of data frames: `sample_coverage`,
#'   `amplicon_coverage`, `allele_data`, `panel_information`,
#'   `collapsed_allele_data`, `resmarker_table` and
#'   `resmarker_microhaplotype_table`.
#' @export
read_mad4hatter_results <- function(results_dir, standardise_sample_name = TRUE,
                                    manifest_sample_names = character()) {
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
    tables <- c("sample_coverage", "amplicon_coverage", "allele_data", "collapsed_allele_data")
    raw_names <- unique(unlist(lapply(results[tables], function(table) table$sample_name)))
    warn_merged_sample_names(raw_names, standardise_illumina_sample_name(raw_names, manifest_sample_names))

    for (name in tables) {
      if (!is.null(results[[name]])) {
        results[[name]]$sample_name <- standardise_illumina_sample_name(results[[name]]$sample_name, manifest_sample_names)
      }
    }
  }

  results
}

warn_merged_sample_names <- function(raw_names, standardised_names) {
  merged <- split(raw_names, standardised_names)
  merged <- merged[lengths(merged) > 1]
  if (length(merged) > 0) {
    examples <- vapply(merged, paste, character(1), collapse = ", ")
    warning(
      "Standardising sample names made different samples identical, so their data will be combined:\n",
      paste0("  ", names(merged), ": ", examples, collapse = "\n"),
      "\nSet standardise_sample_name = FALSE and rename the samples if they are different samples.",
      call. = FALSE
    )
  }
}

#' Compare manifest and pipeline sample names
#'
#' The amplicon coverage contains every sample that went through Mad4hatter,
#' so it is used to check that the manifest and pipeline sample names match.
#' `check_sample_names()` stops with an error if no names match and warns if
#' some samples are only in the manifest or only in the pipeline outputs.
#'
#' @param manifest Manifest from [read_manifest()].
#' @param amplicon_coverage Amplicon coverage from [read_mad4hatter_results()].
#' @return A list with `not_in_results` (manifest samples not in the amplicon
#'   coverage), `not_in_manifest` (amplicon coverage samples not in the
#'   manifest) and `n_matched`. `check_sample_names()` returns it invisibly.
#' @export
sample_name_mismatches <- function(manifest, amplicon_coverage) {
  manifest_names <- unique(manifest$sample_name)
  pipeline_names <- unique(amplicon_coverage$sample_name)
  list(
    not_in_results = setdiff(manifest_names, pipeline_names),
    not_in_manifest = setdiff(pipeline_names, manifest_names),
    n_matched = length(intersect(manifest_names, pipeline_names))
  )
}

#' @rdname sample_name_mismatches
#' @export
check_sample_names <- function(manifest, amplicon_coverage) {
  mismatches <- sample_name_mismatches(manifest, amplicon_coverage)

  if (mismatches$n_matched == 0) {
    stop(
      "None of the sample names in the manifest match the sample names in the Mad4hatter amplicon coverage.\n",
      "  Manifest: ", format_sample_names(unique(manifest$sample_name), 5), "\n",
      "  Amplicon coverage: ", format_sample_names(unique(amplicon_coverage$sample_name), 5), "\n",
      "If the pipeline names still end in sequencer suffixes (e.g. _S1_L001) set standardise_sample_name = TRUE; ",
      "if trimming removed part of the names set standardise_sample_name = FALSE.",
      call. = FALSE
    )
  }
  if (length(mismatches$not_in_results) > 0) {
    warning(
      length(mismatches$not_in_results), " sample(s) in the manifest are not in the Mad4hatter amplicon coverage ",
      "and will be marked for reprep: ", format_sample_names(mismatches$not_in_results),
      call. = FALSE
    )
  }
  if (length(mismatches$not_in_manifest) > 0) {
    warning(
      length(mismatches$not_in_manifest), " sample(s) in the Mad4hatter amplicon coverage are not in the manifest ",
      "and will be left out of the QC: ", format_sample_names(mismatches$not_in_manifest),
      call. = FALSE
    )
  }
  invisible(mismatches)
}

format_sample_names <- function(sample_names, n = 10) {
  shown <- paste(utils::head(sample_names, n), collapse = ", ")
  if (length(sample_names) > n) {
    shown <- paste0(shown, " and ", length(sample_names) - n, " more")
  }
  shown
}
