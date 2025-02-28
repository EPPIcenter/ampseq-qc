# Load required libraries
library(shiny)
library(shinyFiles)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)
library(stringr)

# Source external R scripts
source("R/data_loading.R")
source("R/plots.R")
source("R/utils.R")

# Define UI
ui <- fluidPage(
  
  titlePanel("QC Summary"),
  
  sidebarLayout(
    sidebarPanel(
      # Directory selection button
      shinyDirButton("results_dir", "Select Data Directory", "Choose Folder"),
      verbatimTextOutput("dir_path"),  # Display selected directory path
      
      # Additional file upload
      fileInput("extra_file", "Upload Additional File", accept = c(".txt", ".csv")),
      
      # Slider to adjust read_threshold
      sliderInput("threshold", "Adjust Read Threshold",
                  min = 0, max = 1000, value = 100, step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dimer Content", plotOutput("dimer_plot")), 
        tabPanel("Balancing Plot", plotOutput("balancing_plot")),  
        tabPanel("Amplified Loci Plot", plotOutput("reads_by_amplification_success_plot")), 
        tabPanel("Parasitemia Plot", 
                 uiOutput("parasitemia_plot_ui")),  # Dynamic UI for the Amplified Loci Plot tab
        tabPanel("Plate Layout", plotOutput("plate_layout_plot")),  # New tab for plate map
        tabPanel("Reads Heatmap", plotOutput("plate_read_heatmap")),
        tabPanel("Good Loci Map", plotOutput("plate_good_loci_heatmap"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Set root directory (adjust as needed)
  roots <- c(home = "~")
  shinyDirChoose(input, "results_dir", roots = roots, filetypes = c(""))
  
  # Reactive function to get the directory path
  dir_path <- reactive({
    req(input$results_dir)
    parseDirPath(roots, input$results_dir)
  })
  
  # Display selected directory path
  output$dir_path <- renderText({
    dir_path()
  })
  
  
  # Reactive functions for loading predefined files
  sample_coverage <- reactive({ load_file("sample_coverage.txt", dir_path()) })
  amplicon_coverage <- reactive({ load_file("amplicon_coverage.txt", dir_path()) })
  allele_data <- reactive({ load_file("allele_data.txt", dir_path()) })
  
  # Reactive function to handle the uploaded file
  manifest <- reactive({
    req(input$extra_file)  # Ensure a file is uploaded
    ext <- tools::file_ext(input$extra_file$name)
    
    if (ext == "txt") {
      read.delim(input$extra_file$datapath, sep = "\t")
    } else if (ext == "csv") {
      read.csv(input$extra_file$datapath)
    } else {
      return(NULL)
    }
  })
  
  # Reactive function to merge sample_coverage with uploaded data
  sample_coverage_with_manifest <- reactive({
    req(sample_coverage(), manifest())  # Ensure both files are available
    merge_data(manifest(), sample_coverage())
  })
  
  # Add Columns to amplicons to specify experiment, SampleType
  amplicon_coverage_with_manifest <- reactive({
    req(amplicon_coverage(), manifest()) 
    amplicon_coverage() %>%
      left_join(manifest(), by = c("SampleID")) %>%
      mutate(reaction = str_extract(Locus, "\\d(?=[A-Z]*$)"))
  })
  
  # Create sample summary stats 
  summary_samples <- reactive({
    req(amplicon_coverage_with_manifest()) 
    
    amplicon_coverage_with_manifest() %>%
      group_by(reaction) %>%
      mutate(nreactionloci = n_distinct(Locus)) %>%
      ungroup() %>%
      group_by(SampleID, Batch, SampleType, reaction, nreactionloci) %>%
      summarize(
        reads_per_reaction = sum(OutputPostprocessing, na.rm = TRUE),
        n_good_loci = sum(OutputPostprocessing > input$threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(SampleID, Batch, SampleType) %>%
      mutate(
        reads_per_sample = sum(reads_per_reaction, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(prop_good_loci = n_good_loci / nreactionloci) %>%
      left_join(manifest(), by = join_by(SampleID, Batch, SampleType))
  })
  
  is_field_present <- reactive({
    req(summary_samples())  # Ensure summary_samples is available
    # Check if the necessary fields are present
    "Parasitemia" %in% colnames(summary_samples())
  })
  
  # Render the dimer plot
  output$dimer_plot <- renderPlot({
    req(sample_coverage_with_manifest())  # Ensure merged data exists
    generate_dimer_plot(sample_coverage_with_manifest(), sample_colours)
  })
  
  # Render the balancing plot
  output$balancing_plot <- renderPlot({
    req(summary_samples())  # Ensure summary data exists
    generate_balancing_plot(summary_samples(), sample_colours)
  })

  # Render the reads by amplification success plot
  output$reads_by_amplification_success_plot <- renderPlot({
    req(summary_samples())
    generate_reads_by_amplification_success_plot(summary_samples(), input$threshold, sample_colours)
  })

  # Render the Amplified Loci Plot (only if the required fields are present)
  output$parasitemia_plot_ui <- renderUI({
    if (is_field_present()) {
      plotOutput("parasitemia_plot")
    } else {
      # If fields are missing, display a message instead of the plot
      h4("No parasitemia field in manifest. This plot cannot be generated")
    }
  })

  # Render the reads by amplification success plot
  output$parasitemia_plot <- renderPlot({
    req(summary_samples())
    generate_parasitemia_by_amplification_success_plot(summary_samples(), input$threshold, sample_colours)
  })

  # Render the Plate Layout Plot
  output$plate_layout_plot <- renderPlot({
    req(summary_samples())  # Ensure that summary_samples is available
    batches <- unique(summary_samples()$Batch)  # Assuming 'Batch' is a column in your summary_samples dataset
    
    # Create the plate template
    quadrants <- create_plate_template(summary_samples(),batches)
    
    # Plot the plate layout using the function
    plot_plate_layout(summary_samples(), quadrants, sample_colours)
  })
  
  output$plate_read_heatmap <- renderPlot({
    req(summary_samples())  # Ensure that summary_samples is available
    batches <- unique(summary_samples()$Batch)  # Assuming 'Batch' is a column in your summary_samples dataset
    
    # Create the plate template
    quadrants <- create_plate_template(summary_samples(),batches)
    plot_plate_with_feature(summary_samples(), quadrants, sample_colours, "log10(reads_per_reaction + 0.1)", 2)
  })

  output$plate_good_loci_heatmap <- renderPlot({
    req(summary_samples())  # Ensure that summary_samples is available
    batches <- unique(summary_samples()$Batch)  # Assuming 'Batch' is a column in your summary_samples dataset
    
    # Create the plate template
    quadrants <- create_plate_template(summary_samples(),batches)
    plot_plate_with_feature(summary_samples(),quadrants,  sample_colours, "prop_good_loci", 0.5)
  })

}

# Run the app
shinyApp(ui, server)
