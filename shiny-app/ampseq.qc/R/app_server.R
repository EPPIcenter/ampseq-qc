#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Call data loading module
  data_loading <- mod_data_loading_server("data_loading_1")
  
  # Call parameters module
  params <- mod_parameters_server("parameters_1")
  
  # Call excluded loci module
  excluded_loci <- mod_excluded_loci_server("excluded_loci_1", data_loading$amplicon_coverage)
  
  # Process data
  processed_data <- reactive({
    req(
      data_loading$sample_coverage(), 
      data_loading$amplicon_coverage(), 
      data_loading$allele_data(), 
      data_loading$manifest()
    )
    
    process_data(
      data_loading$sample_coverage(),
      data_loading$amplicon_coverage(),
      data_loading$allele_data(),
      data_loading$manifest(),
      params$standardise_sampleID(),
      params$read_threshold(),
      params$read_filter(),
      params$af_filter(),
      params$negative_control_read_threshold(),
      params$reprep_threshold(),
      params$repool_threshold(),
      params$allele_col(),
      excluded_loci()
    )
  })
  
  # Render plots
  output$dimer_plot <- renderPlot({
    req(processed_data())
    generate_dimer_plot(processed_data()$sample_coverage_with_manifest)
  })
  
  output$balancing_plot <- renderPlot({
    req(processed_data())
    generate_balancing_plot(processed_data()$summary_samples)
  })
  
  # Pre-render the negative control summary plot
  negative_control_summary_plot <- reactive({
    req(processed_data())
    generate_negative_control_summary_plot(processed_data()$amplicons_negative)
  })
  
  output$positive_control_plot <- renderPlotly({
    req(processed_data())
    generate_positive_control_plot(processed_data()$pos_control_data, params$read_filter(), params$af_filter())
  })
  
  output$negative_control_plot <- renderPlot({
    req(processed_data())
    generate_negative_control_plot(processed_data()$amplicons_negative)
  })
  
  output$negative_control_summary_plot <- renderPlotly({
    negative_control_summary_plot()
  })
  
  output$reads_by_amplification_plot <- renderPlot({
    req(processed_data())
    generate_reads_by_amplification_plot(
      processed_data()$summary_samples,
      params$read_threshold(),
      params$reprep_threshold(),
      params$repool_threshold()
    )
  })
  
  output$parasitemia_plot <- renderPlot({
    req(processed_data())
    generate_parasitemia_plot(
      processed_data()$summary_samples,
      params$read_threshold()
    )
  })
  
  output$plate_layout <- renderPlot({
    req(processed_data())
    generate_plate_layout(processed_data()$summary_samples)
  })
  
  output$reads_heatmap <- renderPlot({
    req(processed_data())
    generate_reads_heatmap(processed_data()$summary_samples)
  })
  
  output$amplification_heatmap <- renderPlot({
    req(processed_data())
    generate_amplification_heatmap(processed_data()$summary_samples)
  })
  
  # Render tables
  output$reprep_repool_table <- DT::renderDataTable({
    req(processed_data())
    processed_data()$reprep_repool_summary |>
      dplyr::mutate(
        reads_per_reaction = format(reads_per_reaction, big.mark = ",")
      )
  }, 
  options = list(
    pageLength = 25,
    lengthMenu = c(25, 50, 100),
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ),
  rownames = FALSE,
  filter = 'top'
  )
  
  output$polyclonal_table <- DT::renderDataTable({
    req(processed_data())
    processed_data()$polyclonal_information
  }, 
  options = list(
    pageLength = 25,
    lengthMenu = c(25, 50, 100),
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ),
  rownames = FALSE,
  filter = 'top'
  )
  
  output$negative_control_table <- DT::renderDataTable({
    req(processed_data())
    processed_data()$neg_control_information |>
      dplyr::mutate(
        OutputPostprocessing = format(OutputPostprocessing, big.mark = ",")
      )
  }, 
  options = list(
    pageLength = 25,
    lengthMenu = c(25, 50, 100),
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ),
  rownames = FALSE,
  filter = 'top'
  )
  
  # Render filtered allele data table
  output$filtered_allele_table <- DT::renderDataTable({
    req(data_loading$allele_data(), params$read_filter(), params$af_filter(), params$allele_col())
    
    allele_data <- data_loading$allele_data()
    
    # Group by PseudoCIGAR if selected
    if (params$allele_col() == "PseudoCIGAR") {
      allele_data <- allele_data |>
        dplyr::group_by(SampleID, Locus, PseudoCIGAR) |>
        dplyr::summarise(Reads = sum(Reads), .groups = "drop")
    }
    
    # Calculate allele frequency
    allele_data <- allele_data |>
      dplyr::group_by(SampleID, Locus) |>
      dplyr::mutate(AlleleFreq = Reads / sum(Reads)) |>
      dplyr::ungroup()
    
    # Filter to minimum read and within sample allele frequency
    filtered_data <- allele_data |>
      dplyr::filter(Reads > params$read_filter() & AlleleFreq > params$af_filter())
    
    # Format the data for display
    filtered_data |>
      dplyr::mutate(
        AlleleFreq = round(AlleleFreq * 100, 2),  # Convert to percentage
        Reads = format(Reads, big.mark = ",")     # Add thousands separator
      )
  }, 
  options = list(
    pageLength = 25,
    lengthMenu = c(25, 50, 100, 500),
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ),
  rownames = FALSE,
  filter = 'top'
  )
  
  # Download handler for all tables
  output$download_all_tables <- downloadHandler(
    filename = function() {
      format <- input$summary_tables_format
      if (format == "excel") {
        paste("qc_summary_tables_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
      } else {
        paste("qc_summary_tables_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip", sep = "")
      }
    },
    content = function(file) {
      req(processed_data())
      
      # Create a list of data frames with sheet names
      tables_list <- list(
        "Reprep_Repool_Summary" = processed_data()$reprep_repool_summary,
        "Polyclonal_Information" = processed_data()$polyclonal_information,
        "Negative_Control_Information" = processed_data()$neg_control_information
      )
      
      format <- input$summary_tables_format
      if (format == "excel") {
        # Write all tables to Excel file
        writexl::write_xlsx(tables_list, path = file)
      } else {
        # Create temporary directory for CSV files
        temp_dir <- tempfile("csv_files_")
        dir.create(temp_dir)
        
        # Write each table to a CSV file
        csv_files <- character(length(tables_list))
        for (i in seq_along(tables_list)) {
          name <- names(tables_list)[i]
          # Create a safe filename by replacing spaces and special characters
          safe_name <- gsub("[^[:alnum:]]", "_", name)
          csv_file <- file.path(temp_dir, paste0(safe_name, ".csv"))
          readr::write_csv(tables_list[[i]], csv_file)
          csv_files[i] <- csv_file
        }
        
        # Create zip file using zip::zip()
        zip::zip(file, files = csv_files, mode = "cherry-pick")
        
        # Clean up temporary directory and files
        unlink(temp_dir, recursive = TRUE)
      }
    }
  )

  # Download handler for filtered alleles
  output$download_filtered_alleles <- downloadHandler(
    filename = function() {
      paste0("filtered_alleles_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(data_loading$allele_data(), params$read_filter(), params$af_filter(), params$allele_col())
      
      allele_data <- data_loading$allele_data()
      
      # Group by PseudoCIGAR if selected
      if (params$allele_col() == "PseudoCIGAR") {
        allele_data <- allele_data |>
          dplyr::group_by(SampleID, Locus, PseudoCIGAR) |>
          dplyr::summarise(Reads = sum(Reads), .groups = "drop")
      }
      
      # Calculate allele frequency
      allele_data <- allele_data |>
        dplyr::group_by(SampleID, Locus) |>
        dplyr::mutate(AlleleFreq = Reads / sum(Reads)) |>
        dplyr::ungroup()
      
      # Filter to minimum read and within sample allele frequency
      filtered_data <- allele_data |>
        dplyr::filter(Reads > params$read_filter() & AlleleFreq > params$af_filter()) |>
        dplyr::mutate(AlleleFreq = round(AlleleFreq * 100, 2))  # Convert to percentage
      
      # Write to CSV
      readr::write_csv(filtered_data, file)
    }
  )
}
