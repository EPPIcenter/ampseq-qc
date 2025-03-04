# R/plots.R

sample_colours <- c(
    "negative" = "firebrick3",
    "positive" = "blue3",
    "sample" = "darkgrey",
    "NA" = "white"
)


# Function to generate the dimer plot
generate_dimer_plot <- function(sample_coverage_with_manifest, sample_colours) {
  ggplot(data = sample_coverage_with_manifest) +
    geom_point(aes(x = Input + 0.9, 
                    y = (1 - OutputPostprocessing / Input) * 100, 
                    color = SampleType),
                stroke = 1) +
    scale_x_log10() +
    facet_wrap(~Batch) +
    ylab("% Dimers") +
    xlab("Input Reads") +
    ggtitle("Dimer Content") +
    scale_color_manual(values = sample_colours)
}

# Function to generate the balancing plot
generate_balancing_plot <- function(summary_samples, sample_colours) {
  ggplot() + 
    ggbeeswarm::geom_quasirandom(
    data = summary_samples %>% 
        select(SampleID, Batch, reads_per_sample, SampleType) %>% 
        distinct(), 
    aes(x = Batch, y = reads_per_sample + 0.9, color = SampleType)
    ) +
    scale_y_log10() +
    scale_color_manual(values = sample_colours) +
    ylab("Total Reads for Sample") +
    ggtitle("Balancing Across Batches")
}

# Function to generate the plot for Amplified Loci
generate_reads_by_amplification_success_plot <- function(summary_samples, threshold, sample_colours) {
  ggplot(data = summary_samples) +
    geom_point(aes(x = reads_per_sample + 0.9, 
                    y = n_good_loci, 
                    color = SampleType),
                stroke = 1) +
    scale_x_log10() +
    facet_grid(cols = vars(Batch), rows = vars(reaction), scale = "free_y") +
    ylab(paste0("Amplicons with >", threshold, " reads")) +
    xlab("Total Reads for Sample") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Amplicons with `good` read depth") +
    scale_color_manual(values = sample_colours)
}

create_plate_template <- function(summary_samples, batches, nrows=8, ncols=12) {
    quadrants <-
      expand.grid(
        Batch = batches,
        y = 1:nrows,
        x = 1:ncols
      ) %>%
      mutate(
        ymin = y - 0.45,
        ymax = y + 0.45,
        xmin = x - 0.45,
        xmax = x + 0.45
      ) %>%
      mutate(Row = rev(letters[1:nrows])[y]) %>%
      left_join(
        summary_samples %>%  select(Batch, Column, Row, SampleType) %>% distinct(),
        by = c("Batch", "x" = "Column", "Row" = "Row")
      )
    return(quadrants)
}

plot_plate_layout <- function(summary_samples, quadrants, sample_colours) {
    ggplot(summary_samples) +
      # Tiles representing sample types, fill based on SampleType
      geom_tile(aes(x = Column, y = Row, fill = SampleType), color = "white") +
      # Facet by Batch
      facet_wrap(vars(Batch), scales = "free", ncol = 1) +
      # Quadrants as rectangles with borders
      geom_rect(
        data = quadrants,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        color = "black", fill = NA, linewidth = 1
      ) +
      # Customizing the x-axis
      scale_x_continuous(breaks = 1:12, labels = as.character(1:12)) +
    
      # Custom fill colors for each SampleType
      scale_fill_manual(
        values = sample_colours
      ) +
      # Theming
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 14, face = "bold"), 
        panel.grid = element_blank(),  
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      ) +
      # Title for the plot
    ggtitle("Sample Types and Quadrants Distribution by Batch")
}

plot_plate_with_feature <- function(summary_samples, quadrants, sample_colours, fill_param, scale_midpoint) {
    ggplot(summary_samples) +
      geom_tile(aes(
        x = Column,
        y = Row,
        fill = !!rlang::parse_expr(fill_param),
        color = SampleType  # Add color aesthetic for SampleType
      ), linewidth = 1.2) +  # Adjust size for outline thickness
    
      scale_fill_gradient2(
        low = "black",
        mid = "darkorange4",
        high = "darkorange",
        midpoint = scale_midpoint,
        name = "log10(reads)"
      ) +
      scale_color_manual(
          values = sample_colours
        ) + 
      facet_grid(rows = vars(reaction), cols = vars(Batch)) +
      geom_rect(
        data = quadrants,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
        ),
        fill = NA,
        linewidth = 0.3,
        color = "black"
      ) +

        # Theming
          theme_minimal() +
          theme(
            axis.text = element_text(size = 12),  
            strip.text = element_text(size = 14, face = "bold"), 
            panel.grid = element_blank(),  
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          ) +
      theme(legend.position = "bottom", aspect.ratio = 0.66)
}

# Function to generate the plot
generate_parasitemia_by_amplification_success_plot <- function(summary_samples, threshold, sample_colours) {
    ggplot(data = summary_samples) +
        # Plot points with different colors for each sample type
        geom_point(aes(x = Parasitemia + 0.9, 
                        y = n_good_loci, 
                        color = SampleType),
                    stroke = 1) +
    # Log scale for the x-axis
    scale_x_log10() +
    # Faceting by Batch column
    facet_grid(cols = vars(Batch),rows = vars(reaction),
                scale = "free_y")+
    # Adding labels and title
    ylab(paste0("Amplicons with >",threshold," reads")) +
    xlab("Parasitemia (log10)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+

    ggtitle("Amplicons with `good` read depth")+
    # Color scale for SampleType
    scale_color_manual(values = sample_colours) 
}