#' Parameters Module UI
#' 
#' @param id The module id
#' @return A tagList containing UI elements
#' @noRd
mod_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("read_threshold"), "Read Threshold for Successful Amplification",
                value = 100, min = 0, max = 1000),
    numericInput(ns("read_filter"), "Minimum Reads per ASV (Positive Control Filter)",
                value = 0, min = 0),
    numericInput(ns("af_filter"), "Minimum Allele Frequency per ASV (Positive Control Filter)",
                value = 0.01, min = 0, max = 1, step = 0.01),
    numericInput(ns("negative_control_read_threshold"), "Read Threshold for Negative Controls",
                value = 50, min = 0),
    numericInput(ns("reprep_threshold"), "Reprep Threshold (Proportion of Targets)",
                value = 0.5, min = 0, max = 1, step = 0.1),
    numericInput(ns("repool_threshold"), "Repool Threshold (Proportion of Targets)",
                value = 0.75, min = 0, max = 1, step = 0.1),
    selectInput(ns("allele_col"), "Allele ID Column",
               choices = c("PseudoCIGAR", "ASV"),
               selected = "PseudoCIGAR"),
    checkboxInput(ns("standardise_sampleID"), "Standardize Sample IDs", value = TRUE)
  )
}

#' Parameters Module Server
#' 
#' @param id The module id
#' @return A list of reactive values containing parameter values
#' @noRd
mod_parameters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Return reactive values
    list(
      read_threshold = reactive(input$read_threshold),
      read_filter = reactive(input$read_filter),
      af_filter = reactive(input$af_filter),
      negative_control_read_threshold = reactive(input$negative_control_read_threshold),
      reprep_threshold = reactive(input$reprep_threshold),
      repool_threshold = reactive(input$repool_threshold),
      allele_col = reactive(input$allele_col),
      standardise_sampleID = reactive(input$standardise_sampleID)
    )
  })
} 