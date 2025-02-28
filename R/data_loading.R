# R/data_loading.R
# Function to load predefined files
load_file <- function(file_name, dir) {
    req(dir)
    file_path <- file.path(dir, file_name)

    if (file.exists(file_path)) {
        read.delim(file_path, sep = "\t")
    } else {
        return(NULL)
    }
}

# Function to merge data
merge_data <- function(manifest, sample_coverage) {
  sample_coverage %>%
    mutate(Reads = as.numeric(Reads)) %>% 
    replace_na(list(Reads = 0)) %>%
    pivot_wider(names_from = Stage, values_from = Reads) %>%
    left_join(manifest, by = "SampleID") 
}