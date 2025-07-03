#' Data Loading Module UI
#' 
#' @param id The module id
#' @return A tagList containing UI elements
#' @noRd
mod_data_loading_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyFiles::shinyDirButton(ns("results_dir"), "Select Data Directory", "Choose Folder"),
    verbatimTextOutput(ns("dir_path")),
    fileInput(ns("manifest_file"), "Upload Sample Manifest", 
              accept = c(".csv", ".txt"),
              buttonLabel = "Browse...")
  )
}

#' Data Loading Module Server
#' 
#' @param id The module id
#' @return A list of reactive values containing loaded data
#' @noRd
mod_data_loading_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set root directory based on OS
    roots <- if (.Platform$OS.type == "windows") {
      c(home = "~", root = "C:/")
    } else {
      c(home = "~", root = "/")
    }
    
    # Directory selection without file type restrictions
    shinyFiles::shinyDirChoose(input, "results_dir", roots = roots)
    
    # Clear manifest when directory changes
    observeEvent(input$results_dir, {
      shinyjs::reset("manifest_file")
    })
    
    # Clear directory when manifest changes
    observeEvent(input$manifest_file, {
      shinyjs::reset("results_dir")
    })
    
    # Reactive function to get the directory path
    dir_path <- shiny::reactive({
      shiny::req(input$results_dir)
      path <- shinyFiles::parseDirPath(roots, input$results_dir)
      # Normalize path separators
      normalizePath(path, winslash = "/")
    })
    
    # Display selected directory path
    output$dir_path <- shiny::renderText({
      dir_path()
    })
    
    # Load data files with normalized paths
    sample_coverage <- shiny::reactive({
      shiny::req(dir_path())
      readr::read_tsv(file.path(dir_path(), "sample_coverage.txt"), show_col_types = FALSE)
    })
    
    amplicon_coverage <- shiny::reactive({
      shiny::req(dir_path())
      readr::read_tsv(file.path(dir_path(), "amplicon_coverage.txt"), show_col_types = FALSE)
    })
    
    allele_data <- shiny::reactive({
      shiny::req(dir_path())
      readr::read_tsv(file.path(dir_path(), "allele_data.txt"), show_col_types = FALSE)
    })
    
    manifest <- shiny::reactive({
      shiny::req(input$manifest_file)
      ext <- tools::file_ext(input$manifest_file$name)
      if (ext == "txt") {
        readr::read_tsv(input$manifest_file$datapath, show_col_types = FALSE)
      } else if (ext == "csv") {
        readr::read_csv(input$manifest_file$datapath, show_col_types = FALSE)
      }
    })
    
    # Return reactive values
    list(
      dir_path = dir_path,
      sample_coverage = sample_coverage,
      amplicon_coverage = amplicon_coverage,
      allele_data = allele_data,
      manifest = manifest
    )
  })
} 