default_sample_colours <- c(
  "negative" = "red3",
  "positive" = "blue3",
  "sample" = "darkgrey",
  "empty" = "black"
)

default_reaction_colours <- c("orangered2", "skyblue", "black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray")

#' Plate template
#'
#' Builds one row per well for each batch, joined to the sample type in that
#' well.
#'
#' @param summary_samples Output of [summarise_samples()].
#' @param batches Batches to include.
#' @param nrows,ncols Plate dimensions.
#' @return A data frame of wells with tile coordinates.
#' @export
create_plate_template <- function(summary_samples, batches, nrows = 8, ncols = 12) {
  expand.grid(
    Batch = batches,
    y = 1:nrows,
    Column = 1:ncols
  ) %>%
    dplyr::mutate(
      ymin = y - 0.45,
      ymax = y + 0.45,
      xmin = Column - 0.45,
      xmax = Column + 0.45
    ) %>%
    dplyr::mutate(Row = toupper(rev(letters[1:nrows])[y])) %>%
    dplyr::left_join(
      summary_samples %>%
        dplyr::select(Batch, Column, Row, SampleType) %>%
        dplyr::distinct(),
      by = c("Batch", "Column", "Row" = "Row")
    )
}

#' Plate layout plots
#'
#' @param quadrants_batch Plate template for one batch.
#' @param plate_data Output of [create_plate_template()].
#' @param sample_colours Named colours for each sample type.
#' @param batch_name Batch name for the title.
#' @return `plot_plate()` returns a ggplot; `plot_plate_layouts()` returns a
#'   named list of ggplots, one per batch.
#' @export
plot_plate <- function(quadrants_batch, sample_colours = default_sample_colours, batch_name = "") {
  ggplot2::ggplot(quadrants_batch) +
    ggplot2::geom_tile(ggplot2::aes(x = Column, y = y, fill = SampleType), color = "white") +
    ggplot2::scale_y_continuous(breaks = quadrants_batch$y, labels = quadrants_batch$Row) +
    ggplot2::xlab("Column") +
    ggplot2::ylab("Row") +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = as.character(1:12)) +
    ggplot2::geom_rect(
      data = quadrants_batch,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      color = "black", fill = NA, linewidth = 1
    ) +
    ggplot2::scale_fill_manual(values = sample_colours) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(color = "black", size = 1)
      )
    ) +
    ggplot2::ggtitle(paste0("Plate Map: ", batch_name)) +
    ggplot2::coord_fixed(ratio = 0.75)
}

#' @rdname plot_plate
#' @export
plot_plate_layouts <- function(plate_data, sample_colours = default_sample_colours) {
  plate_maps <- list()
  plate_data$SampleType[is.na(plate_data$SampleType)] <- "empty"
  for (b in unique(plate_data$Batch)) {
    quadrants_batch <- plate_data %>%
      dplyr::filter(Batch == b)
    plate_maps[[b]] <- plot_plate(quadrants_batch, sample_colours, b)
  }
  plate_maps
}

#' Primer dimer content
#'
#' @param sample_coverage_with_manifest Output of [merge_sample_coverage()].
#' @param sample_colours Named colours for each sample type.
#' @return A ggplot of input reads against percentage of dimers per batch.
#' @export
generate_dimer_plot <- function(sample_coverage_with_manifest, sample_colours = default_sample_colours) {
  ggplot2::ggplot(data = sample_coverage_with_manifest %>% dplyr::arrange(dplyr::desc(SampleType))) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = Input + 0.9,
        y = (1 - OutputPostprocessing / Input) * 100,
        color = SampleType
      ),
      shape = 1,
      alpha = 0.8,
      stroke = 1
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::facet_wrap(~Batch) +
    ggplot2::ylab("% Dimers") +
    ggplot2::xlab("Input Reads") +
    ggplot2::ggtitle("Dimer Content") +
    ggplot2::scale_color_manual(values = sample_colours)
}

#' Balancing across batches
#'
#' @param summary_samples Output of [summarise_samples()].
#' @param sample_colours Named colours for each sample type.
#' @return A ggplot of total reads per sample by batch.
#' @export
generate_balancing_plot <- function(summary_samples, sample_colours = default_sample_colours) {
  ggplot2::ggplot() +
    ggbeeswarm::geom_quasirandom(
      data = summary_samples %>%
        dplyr::select(sample_name, Batch, reads_per_sample, SampleType) %>%
        dplyr::distinct(),
      ggplot2::aes(x = Batch, y = reads_per_sample + 0.9, color = SampleType)
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_color_manual(values = sample_colours) +
    ggplot2::ylab("Total Reads for Sample") +
    ggplot2::ggtitle("Balancing Across Batches") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}

#' Alleles per target in positive controls
#'
#' @param category_counts `category_counts` from
#'   [summarise_positive_controls()].
#' @return A ggplot of targets per allele category for each positive control,
#'   with a `text` aesthetic for [plotly::ggplotly()] tooltips.
#' @export
plot_positive_control_alleles <- function(category_counts) {
  fill_colors <- c("Monoallelic" = "Gray", "2 Alleles" = "orangered2", ">2 Alleles" = "cyan")
  ggplot2::ggplot(category_counts, ggplot2::aes(x = sample_name, y = Count, fill = Category, text = paste("Count:", Count))) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::labs(title = "Alleles per Target for Positive Controls", x = "Sample Name", y = "Number of Targets") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}

#' Read distribution per negative control
#'
#' @param data Negative control amplicon coverage, from
#'   [negative_control_amplicons()].
#' @param reaction_colours Colours for each reaction.
#' @param batch_name Optional batch name for the title.
#' @param remove_zeros If `TRUE`, targets with no reads are not plotted.
#' @param ncol_layout Number of facet columns.
#' @return A ggplot, or `NULL` if there is nothing to plot.
#' @export
plot_negative_control_histogram <- function(data, reaction_colours = default_reaction_colours, batch_name = NULL,
                                            remove_zeros = TRUE, ncol_layout = 3) {
  data <- data %>% dplyr::mutate(reaction = factor(reaction))

  if (remove_zeros) {
    data <- data %>% dplyr::filter(!is.na(OutputPostprocessing) & OutputPostprocessing > 0)
  }

  if (nrow(data) == 0) {
    return(NULL)
  }

  max_read <- suppressWarnings(max(data$OutputPostprocessing, na.rm = TRUE))
  if (!is.finite(max_read)) max_read <- 0

  x_max <- if (max_read < 30) 40 else (max_read + 10)
  x_limits <- c(0, x_max)

  total_targets <- nrow(data)
  y_max <- if (total_targets < 90) 10 else (total_targets + 10)
  y_limits <- c(0.01, y_max)

  title_suffix <- if (!is.null(batch_name)) paste0(" - ", batch_name) else ""

  ggplot2::ggplot(data) +
    ggplot2::geom_histogram(ggplot2::aes(x = OutputPostprocessing, fill = reaction), bins = 12, boundary = 0) +
    ggplot2::facet_wrap(~negative, ncol = ncol_layout, scales = "fixed") +
    ggplot2::xlab("Reads") +
    ggplot2::ylab("Number of targets") +
    ggplot2::ggtitle(paste0("Read Distribution per Negative Control", title_suffix)) +
    ggplot2::scale_fill_manual(values = reaction_colours) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      panel.spacing = ggplot2::unit(1, "lines")
    )
}

#' Figure height for negative control histograms
#'
#' @param n_controls Number of negative controls plotted.
#' @param ncol_layout Number of facet columns.
#' @param base_height_per_row Height per row of facets, in inches.
#' @param min_height,max_height Height limits, in inches.
#' @return Figure height in inches.
#' @export
negative_control_plot_height <- function(n_controls, ncol_layout = 3, base_height_per_row = 2.5,
                                         min_height = 6, max_height = 20) {
  min(max_height, max(min_height, ceiling(n_controls / ncol_layout) * base_height_per_row))
}

#' Total reads per target across negative controls
#'
#' @param summary_data Output of [summarise_negative_control_targets()].
#' @param reaction_colours Colours for each reaction.
#' @return A ggplot with a `text` aesthetic for [plotly::ggplotly()] tooltips.
#' @export
plot_negative_control_target_reads <- function(summary_data, reaction_colours = default_reaction_colours) {
  locus_labels <- summary_data %>%
    dplyr::distinct(target_name, Locus_label)

  ggplot2::ggplot(summary_data, ggplot2::aes(
    x = target_name, y = sum_reads, fill = reaction,
    text = paste("Target: ", target_name, "<br>Total Reads: ", sum_reads, "<br>Controls with Reads: ", samples_with_reads)
  )) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::xlab("Target (Chromosome:Start Coordinate)") +
    ggplot2::ylab("Sum of Reads Across Controls") +
    ggplot2::ggtitle("Total Reads per Target Across Negative Controls") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8),
      axis.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::scale_x_discrete(labels = stats::setNames(locus_labels$Locus_label, locus_labels$target_name)) +
    ggplot2::scale_fill_manual(values = reaction_colours)
}

#' Parasitemia against amplification success
#'
#' @param summary_samples_for_batch,summary_samples Output of
#'   [summarise_samples()], for one batch or all batches.
#' @param threshold Reads needed to count a target as amplified, for the axis
#'   label.
#' @param sample_colours Named colours for each sample type.
#' @param batch_name Batch name for the title.
#' @return A ggplot, or a named list of ggplots (one per batch) for the
#'   plural function.
#' @export
generate_parasitemia_by_amplification_success_plot <- function(summary_samples_for_batch, threshold,
                                                               sample_colours = default_sample_colours, batch_name = "") {
  ggplot2::ggplot(data = summary_samples_for_batch) +
    ggplot2::geom_point(
      ggplot2::aes(x = Parasitemia + 0.9, y = prop_good_loci, color = SampleType),
      shape = 1,
      alpha = 0.8,
      stroke = 1
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::ylim(0, 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(reaction), scales = "free_y") +
    ggplot2::ylab(paste0("Amplicons with >", threshold, " reads")) +
    ggplot2::xlab("Parasitemia (log10)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(paste0(batch_name, " Amplicons with `good` read depth")) +
    ggplot2::scale_color_manual(values = sample_colours)
}

#' @rdname generate_parasitemia_by_amplification_success_plot
#' @export
generate_parasitemia_by_amplification_success_plots <- function(summary_samples, threshold, sample_colours = default_sample_colours) {
  plots <- list()
  for (b in unique(summary_samples$Batch)) {
    summary_samples_batch <- summary_samples %>%
      dplyr::filter(Batch == b)
    plots[[b]] <- generate_parasitemia_by_amplification_success_plot(summary_samples_batch, threshold, sample_colours, b)
  }
  plots
}

#' Sample reads against amplification success
#'
#' @inheritParams generate_parasitemia_by_amplification_success_plot
#' @param amplification_threshold,read_threshold Reads needed to count a target
#'   as amplified, for the axis label.
#' @param reprep_threshold,repool_threshold QC thresholds drawn as dashed lines.
#' @return A ggplot, or a named list of ggplots (one per batch) for the
#'   plural function.
#' @export
generate_reads_by_amplification_success_plot <- function(summary_samples_for_batch, amplification_threshold,
                                                         sample_colours = default_sample_colours, reprep_threshold = 0.5,
                                                         repool_threshold = 0.75, batch_name = "") {
  ggplot2::ggplot(data = summary_samples_for_batch) +
    ggplot2::geom_point(
      ggplot2::aes(x = reads_per_sample + 0.9, y = prop_good_loci, color = SampleType),
      shape = 1,
      alpha = 0.8,
      stroke = 1
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::facet_grid(rows = ggplot2::vars(reaction), scales = "free_y") +
    ggplot2::ylab(paste0("Amplicons with >", amplification_threshold, " reads")) +
    ggplot2::xlab("Total Reads for Sample") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(paste0(batch_name, " Targets that Amplified Successfully")) +
    ggplot2::scale_color_manual(values = sample_colours) +
    ggplot2::geom_hline(yintercept = reprep_threshold, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = repool_threshold, linetype = "dashed") +
    ggplot2::annotate("text",
      x = min(summary_samples_for_batch$reads_per_sample, na.rm = TRUE),
      y = reprep_threshold, label = "Reprep", vjust = 1, hjust = 0
    ) +
    ggplot2::annotate("text",
      x = min(summary_samples_for_batch$reads_per_sample, na.rm = TRUE),
      y = repool_threshold, label = "Repool", vjust = 1, hjust = 0
    )
}

#' @rdname generate_reads_by_amplification_success_plot
#' @export
generate_reads_by_amplification_success_plots <- function(summary_samples, read_threshold, sample_colours = default_sample_colours,
                                                          reprep_threshold = 0.5, repool_threshold = 0.75) {
  plots <- list()
  for (b in unique(summary_samples$Batch)) {
    summary_samples_batch <- summary_samples %>%
      dplyr::filter(Batch == b)
    plots[[b]] <- generate_reads_by_amplification_success_plot(
      summary_samples_batch, read_threshold, sample_colours,
      reprep_threshold, repool_threshold, b
    )
  }
  plots
}

#' Plate heatmaps
#'
#' @param quadrants_batch Plate data for one batch joined to sample summaries.
#' @param summary_samples Output of [summarise_samples()].
#' @param quadrants Output of [create_plate_template()].
#' @param sample_colours Named colours for each sample type, used for well
#'   outlines.
#' @param batch_name Batch name.
#' @param fill_param Expression, as a string, used for the fill, e.g.
#'   `"prop_good_loci"`.
#' @param scale_midpoint Midpoint of the fill scale.
#' @param scale_label Legend title.
#' @return `plot_plate_with_feature()` returns a ggplot;
#'   `plot_plate_heat_maps()` returns a named list of ggplots, one per batch.
#' @export
plot_plate_with_feature <- function(quadrants_batch, sample_colours = default_sample_colours, batch_name = "",
                                    fill_param, scale_midpoint, scale_label) {
  ggplot2::ggplot(quadrants_batch) +
    ggplot2::geom_tile(
      ggplot2::aes(x = Column, y = y, fill = !!rlang::parse_expr(fill_param)),
      color = NA, width = 0.95, height = 0.95
    ) +
    ggplot2::geom_rect(
      data = quadrants_batch,
      ggplot2::aes(xmin = Column - 0.45, xmax = Column + 0.45, ymin = y - 0.45, ymax = y + 0.45, color = SampleType),
      fill = NA, linewidth = 1.3
    ) +
    ggplot2::facet_grid(reaction ~ Batch) +
    ggplot2::scale_fill_gradient2(
      low = "black",
      mid = "darkorange4",
      high = "darkorange",
      midpoint = scale_midpoint,
      name = scale_label
    ) +
    ggplot2::scale_color_manual(values = sample_colours, na.translate = TRUE) +
    ggplot2::scale_y_continuous(breaks = quadrants_batch$y, labels = quadrants_batch$Row) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = as.character(1:12)) +
    ggplot2::xlab("Column") +
    ggplot2::ylab("Row") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      legend.key = ggplot2::element_rect(colour = "black")
    ) +
    ggplot2::coord_fixed(ratio = 0.75)
}

#' @rdname plot_plate_with_feature
#' @export
plot_plate_heat_maps <- function(summary_samples, quadrants, sample_colours = default_sample_colours,
                                 fill_param, scale_midpoint, scale_label) {
  plate_maps <- list()

  quadrants_by_reaction <- tidyr::crossing(summary_samples %>% dplyr::select(reaction) %>% dplyr::distinct(), quadrants)
  merged_data <- merge(summary_samples, quadrants_by_reaction, by = c("Batch", "Row", "Column", "SampleType", "reaction"), all.y = TRUE)

  merged_data$SampleType[is.na(merged_data$SampleType)] <- "empty"

  for (b in unique(merged_data$Batch)) {
    merged_data_batch <- merged_data %>%
      dplyr::filter(Batch == b)
    plate_maps[[b]] <- plot_plate_with_feature(merged_data_batch, sample_colours, b, fill_param, scale_midpoint, scale_label)
  }
  plate_maps
}

#' QC status by batch
#'
#' @param qc_summary_by_batch Output of [qc_summary_by_batch()].
#' @return A stacked bar chart of the percentage of samples in each status.
#' @export
plot_qc_status_by_batch <- function(qc_summary_by_batch) {
  qc_colors <- c("pass" = "Gray", "repool" = "skyblue", "reprep" = "orangered2")

  ggplot2::ggplot(qc_summary_by_batch, ggplot2::aes(x = Batch, y = percentage, fill = status)) +
    ggplot2::geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(percentage > 5, paste0(round(percentage, 1), "%"), "")),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 5,
      fontface = "bold",
      color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = qc_colors,
      name = "QC Status",
      breaks = c("pass", "repool", "reprep"),
      labels = c("Pass", "Repool", "Reprep")
    ) +
    ggplot2::labs(title = "QC Status by Batch", x = "Batch", y = "Percentage of Samples (%)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "top",
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::ylim(0, 100)
}
