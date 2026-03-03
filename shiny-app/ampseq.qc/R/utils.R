# Function to check if a file exists in a directory
file_exists <- function(filename, directory) {
  normalizePath(file.path(directory, filename), mustWork = FALSE) |> file.exists()
}

# Function to validate manifest file
validate_manifest <- function(manifest) {
  required_fields <- c("SampleID", "SampleType", "Batch", "Column", "Row")
  missing_fields <- setdiff(required_fields, names(manifest))
  
  if (length(missing_fields) > 0) {
    stop("Manifest file is missing required fields: ", 
         paste(missing_fields, collapse = ", "))
  }
  
  # Validate SampleType values
  valid_types <- c("sample", "positive", "negative")
  invalid_types <- setdiff(unique(manifest$SampleType), valid_types)
  
  if (length(invalid_types) > 0) {
    stop("Invalid SampleType values found: ", 
         paste(invalid_types, collapse = ", "),
         ". Valid values are: ", 
         paste(valid_types, collapse = ", "))
  }
  
  # Validate Column and Row values
  if (!all(manifest$Column %in% 1:12)) {
    stop("Column values must be between 1 and 12")
  }
  
  if (!all(toupper(manifest$Row) %in% LETTERS[1:8])) {
    stop("Row values must be letters A through H")
  }
  
  TRUE
}

# Function to standardize sample IDs
standardize_sample_id <- function(sample_id) {
  stringr::str_replace(sample_id, "_L001$", "") |>
    stringr::word(start = 1, end = -2, sep = "_")
}

# Function to format numbers for display
format_number <- function(x, digits = 2) {
  if (is.numeric(x)) {
    format(round(x, digits), nsmall = digits)
  } else {
    x
  }
}

# Function to create a summary of the data
create_data_summary <- function(sample_coverage, amplicon_coverage, allele_data, manifest) {
  list(
    n_samples_manifest = nrow(manifest),
    n_samples_coverage = length(unique(sample_coverage$SampleID)),
    n_samples_amplicon = length(unique(amplicon_coverage$SampleID)),
    n_samples_allele = length(unique(allele_data$SampleID)),
    n_loci = length(unique(amplicon_coverage$Locus)),
    n_batches = length(unique(manifest$Batch)),
    sample_types = table(manifest$SampleType)
  )
}

# Function to check for missing data
check_missing_data <- function(sample_coverage, amplicon_coverage, allele_data, manifest) {
  manifest_samples <- unique(manifest$SampleID)
  coverage_samples <- unique(sample_coverage$SampleID)
  amplicon_samples <- unique(amplicon_coverage$SampleID)
  allele_samples <- unique(allele_data$SampleID)
  
  list(
    missing_in_coverage = setdiff(manifest_samples, coverage_samples),
    missing_in_amplicon = setdiff(manifest_samples, amplicon_samples),
    missing_in_allele = setdiff(manifest_samples, allele_samples)
  )
}

# Function to create a plate map template
create_plate_map <- function(nrows = 8, ncols = 12) {
  # validate nrows and ncols > 0
  if (nrows <= 0 || ncols <= 0) {
    stop("nrows and ncols must be greater than 0")
  }
  expand.grid(
    Row = LETTERS[1:nrows],
    Column = 1:ncols
  ) |>
    dplyr::mutate(
      Well = paste0(Row, Column),
      x = Column,
      y = match(Row, LETTERS[1:nrows])
    )
}

# Function to calculate summary statistics for a numeric vector
calculate_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}

# Function to create a color gradient
create_color_gradient <- function(n, start_color = "black", end_color = "white") {
  colorRampPalette(c(start_color, end_color))(n)
}

# Function to format table output
format_table <- function(df, digits = 2) {
  df |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~format_number(., digits)))
}

# Function to create a download handler for tables
create_download_handler <- function(data, filename, type = "csv") {
  if (type == "csv") {
    function() {
      readr::write_csv(data, normalizePath(filename, mustWork = FALSE))
    }
  } else if (type == "tsv") {
    function() {
      readr::write_tsv(data, normalizePath(filename, mustWork = FALSE))
    }
  }
}

# Function to create a download handler for plots
create_plot_download_handler <- function(plot, filename, width = 10, height = 8) {
  function() {
    ggplot2::ggsave(normalizePath(filename, mustWork = FALSE), plot, width = width, height = height)
  }
} 