# Define color palettes
sample_colours <- c(
  "negative" = "red3",
  "positive" = "blue3",
  "sample" = "darkgrey",
  "empty" = "black"
)

reaction_colours <- c("skyblue", "orangered2", "turqoise3")

# Custom theme for all plots
qc_theme <- function() {
  theme_minimal() +
    theme(
      # Text elements
      text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      
      # Grid and background
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      
      # # Legend
      # legend.position = "bottom",
      # legend.box = "horizontal",
      # legend.margin = margin(t = 10),
      
      # Facets
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      
      # Margins and spacing
      plot.margin = margin(20, 20, 20, 20),
      panel.spacing = unit(1, "lines")
    )
}

# Function to generate dimer plot
generate_dimer_plot <- function(sample_coverage_with_manifest) {
  dat <- sample_coverage_with_manifest |> dplyr::arrange(dplyr::desc(SampleType))
  ggplot(data = dat) +
    geom_point(aes(x = Input + 0.9, 
                   y = (1 - OutputPostprocessing / Input) * 100, 
                   color = SampleType),
               shape = 1, 
               alpha = 0.8,
               stroke = 1) +
    scale_x_log10() +
    facet_wrap(~Batch) +
    ylab("% Dimers") +
    xlab("Input Reads") +
    ggtitle("Dimer Content") +
    scale_color_manual(values = sample_colours) +
    qc_theme()
}

# Function to generate balancing plot
generate_balancing_plot <- function(summary_samples) {
  ggplot() + 
    ggbeeswarm::geom_quasirandom(
      data = summary_samples |>
        dplyr::select(SampleID, Batch, reads_per_sample, SampleType) |>
        dplyr::distinct(), 
      aes(x = Batch, y = reads_per_sample + 0.9, color = SampleType)
    ) +
    scale_y_log10() +
    scale_color_manual(values = sample_colours) +
    ylab("Total Reads for Sample") +
    ggtitle("Balancing Across Batches") +
    qc_theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Function to generate positive control plot
generate_positive_control_plot <- function(pos_control_data, read_filter, af_filter) {
  # Count loci per sample for each category
  result <- pos_control_data |>
    dplyr::group_by(SampleID, Locus) |>
    dplyr::summarise(
      NumASVs_Meeting_Threshold = sum(Reads > read_filter & AlleleFreq > af_filter),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Category = dplyr::case_when(
        NumASVs_Meeting_Threshold == 1 ~ "Monoclonal",
        NumASVs_Meeting_Threshold == 2 ~ "2 Alleles",
        NumASVs_Meeting_Threshold > 2  ~ ">2 Alleles"
      )
    ) |>
    dplyr::group_by(SampleID, Category) |>
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")
  
  # Create ggplot
  p <- ggplot(result, aes(x = SampleID, y = Count, fill = Category, text = paste("Count:", Count))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Clonality of Loci for Positive Controls", x = "Sample ID", y = "Number of Loci") +
    scale_fill_manual(values = c("Monoclonal" = "Gray", "2 Alleles" = "orangered2", ">2 Alleles" = "turquoise3")) + 
    qc_theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Convert to interactive plot
  ggplotly(p, tooltip = "text", width = 800, height = 600)
}

# Function to generate negative control plot
generate_negative_control_plot <- function(amplicons_negative) {
  ggplot(amplicons_negative) +
    geom_histogram(aes(x = OutputPostprocessing, fill = reaction)) +
    facet_wrap(~ negative) +
    xlim(-ifelse(
      max(amplicons_negative$OutputPostprocessing) == 0,
      1e2,
      max(amplicons_negative$OutputPostprocessing)
    ) / 50,
    ifelse(
      max(amplicons_negative$OutputPostprocessing) == 0,
      1e2,
      max(amplicons_negative$OutputPostprocessing) + 10 
    )) +
    xlab("Reads") +
    ylab("Count") + 
    scale_fill_manual(values = reaction_colours) +
    qc_theme()
}

# Function to generate negative control summary plot
generate_negative_control_summary_plot <- function(amplicons_negative) {
  # Get reads for each locus, label reaction and extract chrom and start from locus name
  summary_data <- amplicons_negative |>
    dplyr::group_by(reaction, Locus) |>
    dplyr::summarise(
      sum_reads = sum(Reads, na.rm = TRUE),
      n_samples_with_reads = dplyr::n_distinct(SampleID[Reads > 0]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Chromosome = sub("-.*", "", Locus),  
      Start = as.numeric(sub(".*-(\\d+)-.*", "\\1", Locus))
    ) |>
    dplyr::arrange(Chromosome, Start) |>
    dplyr::mutate(Locus = factor(Locus, levels = unique(Locus))) |>  
    tibble::rowid_to_column("RowID") 
  
  # Reduce x-axis labels: label every nth locus
  nth_label <- 20 
  summary_data <- summary_data |>
    dplyr::mutate(Locus_label = ifelse(RowID %% nth_label == 1, paste0(Chromosome, ":", Start), ""))
  
  # Create the bar plot with simplified hover text
  p <- ggplot(summary_data, aes(x = Locus, y = sum_reads, fill = reaction, 
                               text = paste("Locus:", Locus, 
                                          "<br>Total Reads:", sum_reads,
                                          "<br>Samples with Reads:", n_samples_with_reads))) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Locus (Chromosome:Start Coordinate)") +
    ylab("Sum of Reads Across Controls") +
    ggtitle("Total Reads per Locus Across Negative Controls") +
    scale_x_discrete(labels = summary_data$Locus_label) +
    scale_fill_manual(values = reaction_colours) +
    qc_theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
  
  # Convert to interactive plotly plot with optimized settings
  ggplotly(p, tooltip = "text", width = 800, height = 600) |>
    plotly::config(displayModeBar = FALSE) |>  # Remove mode bar for cleaner look
    plotly::layout(
      hovermode = "closest",  # Optimize hover performance
      margin = list(l = 50, r = 50, b = 100, t = 50)  # Adjust margins for better fit
    )
}

# Function to generate reads by amplification success plot
generate_reads_by_amplification_plot <- function(summary_samples, read_threshold, reprep_threshold, repool_threshold) {
  ggplot(data = summary_samples) +
    geom_point(aes(x = reads_per_sample + 0.9,  
                   y = prop_good_loci,
                   color = SampleType),
               shape = 1,
               alpha = 0.8,
               stroke = 1) +
    scale_x_log10() +
    scale_y_continuous(limits = c(0, 1)) +
    facet_grid(cols = vars(Batch), rows = vars(reaction), scale = "free_y") +
    ylab(paste0("Amplicons with >", read_threshold, " reads")) +
    xlab("Total Reads for Sample") +
    ggtitle("Targets that Amplified Successfully") +
    scale_color_manual(values = sample_colours) +
    geom_hline(yintercept = reprep_threshold, linetype = "dashed") +
    geom_hline(yintercept = repool_threshold, linetype = "dashed") +
    annotate("text", x = min(summary_samples$reads_per_sample, na.rm = TRUE), 
             y = reprep_threshold, label = "Reprep", vjust = 1, hjust = 0) +
    annotate("text", x = min(summary_samples$reads_per_sample, na.rm = TRUE), 
             y = repool_threshold, label = "Repool", vjust = 1, hjust = 0) +
    qc_theme()
}

# Function to generate parasitemia plot
generate_parasitemia_plot <- function(summary_samples, read_threshold) {
  ggplot(data = summary_samples) +
    geom_point(aes(x = Parasitemia + 0.9, 
                   y = prop_good_loci, 
                   color = SampleType),
               shape = 1,
               alpha = 0.8,
               stroke = 1) +
    scale_x_log10() +
    ylim(0, 1) + 
    facet_grid(cols = vars(Batch), rows = vars(reaction), scale = "free_y") +
    ylab(paste0("Amplicons with >", read_threshold, " reads")) +
    xlab("Parasitemia (log10)") +
    ggtitle("Amplicons with `good` read depth") +
    scale_color_manual(values = sample_colours) +
    qc_theme()
}

# Function to create plate template
create_plate_template <- function(summary_samples, batches, nrows = 8, ncols = 12) {
  expand.grid(
    Batch = batches,
    y = 1:nrows,
    Column = 1:ncols
  ) |>
    dplyr::mutate(
      ymin = y - 0.45,
      ymax = y + 0.45,
      xmin = Column - 0.45,
      xmax = Column + 0.45
    ) |>
    dplyr::mutate(Row = toupper(rev(letters[1:nrows])[y])) |>
    dplyr::left_join(
      summary_samples |>
        dplyr::select(Batch, Column, Row, SampleType) |>
        dplyr::distinct(),
      by = c("Batch", "Column", "Row" = "Row")
    )
}

# Function to generate plate layout
generate_plate_layout <- function(summary_samples) {
  batches <- unique(summary_samples$Batch)
  quadrants <- create_plate_template(summary_samples, batches)
  
  # Ensure that NAs in SampleType are replaced with "empty" string
  quadrants$SampleType[is.na(quadrants$SampleType)] <- "empty"
  
  ggplot(quadrants) +
    geom_tile(aes(x = Column, y = y, fill = SampleType), color = "white") +
    facet_wrap(vars(Batch), ncol = 1) +
    scale_y_continuous(breaks = quadrants$y, labels = quadrants$Row) +
    xlab("Column") +
    ylab("Row") +
    scale_x_continuous(breaks = 1:12, labels = as.character(1:12)) +
    geom_rect(
      data = quadrants,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      color = "black", fill = NA, linewidth = 1
    ) +
    scale_fill_manual(values = sample_colours) +
    ggtitle("Sample Types and Quadrants Distribution by Batch") +
    coord_fixed(ratio = 0.75) +
    qc_theme()
}

# Function to generate reads heatmap
generate_reads_heatmap <- function(summary_samples) {
  batches <- unique(summary_samples$Batch)
  quadrants <- create_plate_template(summary_samples, batches)
  
  merged_data <- merge(summary_samples, quadrants, by = c("Batch", "Row", "Column", "SampleType"), all.x = TRUE)
  merged_data$SampleType[is.na(merged_data$SampleType)] <- "empty"
  
  ggplot(merged_data) +
    geom_tile(aes(x = Column, y = y, fill = log10(reads_per_reaction + 0.1)), color = NA, width = 0.95, height = 0.95) +
    geom_rect(data = merged_data,
              aes(xmin = Column-0.45, xmax = Column+0.45, ymin = y-0.45, ymax = y+0.45, color = SampleType),
              fill = NA, linewidth = 1.3) +
    facet_grid(reaction ~ Batch) +
    scale_fill_gradient2(
      low = "black",
      mid = "darkorange4",
      high = "darkorange",
      midpoint = 2,
      name = "log10(reads)"
    ) +
    scale_color_manual(values = sample_colours, na.translate = TRUE) +
    scale_y_continuous(breaks = merged_data$y, labels = merged_data$Row) +
    scale_x_continuous(breaks = 1:12, labels = as.character(1:12)) +
    xlab("Column") +
    ylab("Row") +
    ggtitle("Reads per Reaction Heatmap") +
    coord_fixed(ratio = 0.75) +
    qc_theme()
}

# Function to generate amplification heatmap
generate_amplification_heatmap <- function(summary_samples) {
  batches <- unique(summary_samples$Batch)
  quadrants <- create_plate_template(summary_samples, batches)
  
  merged_data <- merge(summary_samples, quadrants, by = c("Batch", "Row", "Column", "SampleType"), all.x = TRUE)
  merged_data$SampleType[is.na(merged_data$SampleType)] <- "empty"
  
  ggplot(merged_data) +
    geom_tile(aes(x = Column, y = y, fill = prop_good_loci), color = NA, width = 0.95, height = 0.95) +
    geom_rect(data = merged_data,
              aes(xmin = Column-0.45, xmax = Column+0.45, ymin = y-0.45, ymax = y+0.45, color = SampleType),
              fill = NA, linewidth = 1.3) +
    facet_grid(reaction ~ Batch) +
    scale_fill_gradient2(
      low = "black",
      mid = "darkorange4",
      high = "darkorange",
      midpoint = 0.5,
      name = "Proportion of Good Loci"
    ) +
    scale_color_manual(values = sample_colours, na.translate = TRUE) +
    scale_y_continuous(breaks = merged_data$y, labels = merged_data$Row) +
    scale_x_continuous(breaks = 1:12, labels = as.character(1:12)) +
    xlab("Column") +
    ylab("Row") +
    ggtitle("Amplification Success Heatmap") +
    coord_fixed(ratio = 0.75) +
    qc_theme()
} 