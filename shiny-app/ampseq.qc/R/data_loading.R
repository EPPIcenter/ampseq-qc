# Function to process all data
process_data <- function(sample_coverage, amplicon_coverage, allele_data, manifest,
                        standardise_sampleID, read_threshold, read_filter,
                        af_filter, negative_control_read_threshold,
                        reprep_threshold, repool_threshold, allele_col,
                        excluded_loci) {
  
  # Standardize sample IDs if requested
  if (standardise_sampleID) {
    sample_coverage <- sample_coverage |> 
      dplyr::mutate(SampleID = stringr::word(sub("_L001$", "", SampleID), 
                                            start = 1, end = -2, sep = "_"))
    
    amplicon_coverage <- amplicon_coverage |>
      dplyr::mutate(SampleID = stringr::word(sub("_L001$", "", SampleID), 
                                            start = 1, end = -2, sep = "_"))
    
    allele_data <- allele_data |>
      dplyr::mutate(SampleID = stringr::word(sub("_L001$", "", SampleID), 
                                            start = 1, end = -2, sep = "_"))
  }
  
  # Filter out excluded loci
  amplicon_coverage <- amplicon_coverage |>
    dplyr::filter(!Locus %in% excluded_loci)
  
  allele_data <- allele_data |>
    dplyr::filter(!Locus %in% excluded_loci)

  nloci_table <- allele_data |>
    dplyr::distinct(Locus) |>
    dplyr::mutate(reaction = stringr::str_extract(Locus, "\\d(?=[A-Z]*$)")) |>
    dplyr::count(reaction) |>
    dplyr::rename(nreactionloci = n)

  # Merge data with manifest
  sample_coverage_with_manifest <- sample_coverage |>
    dplyr::mutate(Reads = as.numeric(Reads)) |>
    tidyr::replace_na(list(Reads = 0)) |>
    tidyr::pivot_wider(names_from = Stage, values_from = Reads) |>
    dplyr::inner_join(manifest, by = "SampleID")
  
  amplicon_coverage_with_manifest <- amplicon_coverage |>
    dplyr::mutate(reaction = stringr::str_extract(Locus, "\\d(?=[A-Z]*$)")) |>
    dplyr::inner_join(manifest, by = "SampleID")
  
  # Create summary statistics
  summary_samples <- create_summary_stats(amplicon_coverage_with_manifest, nloci_table, read_threshold) |>
    dplyr::inner_join(manifest, by = c("SampleID", "Batch", "SampleType"))
  
  # Process positive control data
  pos_control_data <- process_positive_controls(allele_data, manifest, allele_col, read_filter, af_filter)
  
  # Process negative control data
  amplicons_negative <- process_negative_controls(amplicon_coverage_with_manifest)
  
  # Generate summary tables
  reprep_repool_summary <- generate_reprep_repool_table(summary_samples, reprep_threshold, repool_threshold)
  polyclonal_information <- generate_polyclonal_info(pos_control_data, read_filter, af_filter)
  neg_control_information <- generate_negative_control_info(amplicons_negative, negative_control_read_threshold)
  
  # Return all processed data
  list(
    sample_coverage_with_manifest = sample_coverage_with_manifest,
    summary_samples = summary_samples,
    pos_control_data = pos_control_data,
    amplicons_negative = amplicons_negative,
    reprep_repool_summary = reprep_repool_summary,
    polyclonal_information = polyclonal_information,
    neg_control_information = neg_control_information
  )
}


# Function to merge data with manifest
merge_data <- function(manifest, sample_coverage) {
  sample_coverage |>
    dplyr::mutate(Reads = as.numeric(Reads)) |>
    tidyr::replace_na(list(Reads = 0)) |>
    tidyr::pivot_wider(names_from = Stage, values_from = Reads) |>
    dplyr::inner_join(manifest, by = "SampleID")
}

# Function to create summary statistics
create_summary_stats <- function(amplicon_coverage_with_manifest, nloci_table, read_threshold) {
  amplicon_coverage_with_manifest |>
    dplyr::left_join(nloci_table, by = "reaction") |>
    dplyr::group_by(SampleID, Batch, SampleType, reaction, nreactionloci) |>
    dplyr::summarize(
      reads_per_reaction = sum(OutputPostprocessing),
      n_good_loci = sum(OutputPostprocessing > read_threshold),
      .groups = "drop"
    ) |>
    dplyr::group_by(SampleID, Batch, SampleType) |>
    dplyr::mutate(
      reads_per_sample = sum(reads_per_reaction)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(prop_good_loci = n_good_loci / nreactionloci)
}

# Function to process positive controls
process_positive_controls <- function(allele_data, manifest, allele_col, read_filter, af_filter) {
  # Calculate allele frequencies
  allele_data <- allele_data |>
    dplyr::group_by(SampleID, Locus) |>
    dplyr::mutate(AlleleFreq = Reads / sum(Reads)) |>
    dplyr::ungroup()

  pos_controls <- manifest |>
    dplyr::filter(SampleType == "positive")
  
  # Filter for positive controls
  pos_control_data <- allele_data |>
    dplyr::inner_join(pos_controls, by = "SampleID")
  
  # Group by PseudoCIGAR if selected
  if (allele_col == "PseudoCIGAR") {
    pos_control_data <- pos_control_data |>
      dplyr::group_by(SampleID, Locus, PseudoCIGAR) |>
      dplyr::summarise(
        Reads = sum(Reads), 
        AlleleFreq = sum(AlleleFreq), 
        .groups = "drop"
      )
  }
  
  pos_control_data
}

# Function to process negative controls
process_negative_controls <- function(amplicon_coverage_with_manifest) {
  amplicon_coverage_with_manifest |>
    dplyr::filter(SampleType == "negative") |>
    dplyr::mutate(negative = paste0(Batch, " Well: ", toupper(Row), Column))
}

# Function to generate reprep/repool summary table
generate_reprep_repool_table <- function(summary_samples, reprep_threshold, repool_threshold) {
  summary_samples |>
    dplyr::mutate(
      status = dplyr::case_when(
        prop_good_loci < reprep_threshold ~ "reprep",
        prop_good_loci < repool_threshold ~ "repool",
        TRUE ~ "pass"
      ),
      reason = dplyr::case_when(
        prop_good_loci < reprep_threshold ~ paste("< reprep threshold", reprep_threshold),
        prop_good_loci < repool_threshold ~ paste("< repool threshold", repool_threshold),
        TRUE ~ "NA"
      )
    ) |>
    dplyr::arrange(dplyr::desc(status)) |>
    dplyr::select(SampleID, Batch, reaction, status, reason, reads_per_reaction)
}

# Function to generate polyclonal information
generate_polyclonal_info <- function(pos_control_data, read_filter, af_filter) {
  pos_control_data |>
    dplyr::group_by(SampleID, Locus) |>
    dplyr::summarise(
      NumASVs_Meeting_Threshold = sum(Reads > read_filter & AlleleFreq > af_filter),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Category = dplyr::case_when(
        NumASVs_Meeting_Threshold == 1 ~ "Monoclonal",
        NumASVs_Meeting_Threshold == 2 ~ "2 Alleles",
        NumASVs_Meeting_Threshold > 2 ~ ">2 Alleles"
      )
    )
}

# Function to generate negative control information
generate_negative_control_info <- function(amplicons_negative, negative_control_read_threshold) {
  amplicons_negative |>
    dplyr::filter(OutputPostprocessing > negative_control_read_threshold) |>
    dplyr::select(SampleID, Locus, OutputPostprocessing)
} 