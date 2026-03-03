#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("QC Summary"),
      
      sidebarLayout(
        sidebarPanel(
          # Data loading module
          mod_data_loading_ui("data_loading_1"),
          
          # Parameters module
          mod_parameters_ui("parameters_1"),
          
          # Excluded loci module
          mod_excluded_loci_ui("excluded_loci_1")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Dimer Content", 
                    fluidRow(
                      column(12, plotOutput("dimer_plot", width = "100%", height = "600px"))
                    )),
            tabPanel("Balancing Plot", 
                    fluidRow(
                      column(12, plotOutput("balancing_plot", width = "100%", height = "600px"))
                    )),
            tabPanel("Control Summary",
                     tabsetPanel(
                       tabPanel("Positive Controls", 
                               fluidRow(
                                 column(12, plotlyOutput("positive_control_plot", width = "800px", height = "600px"))
                               )),
                       tabPanel("Negative Controls", 
                               fluidRow(
                                 column(12, plotOutput("negative_control_plot", width = "100%", height = "400px")),
                                 column(12, plotlyOutput("negative_control_summary_plot", width = "800px", height = "600px"))
                               ))
                     )),
            shiny::tabPanel("Amplification Success",
                     shiny::tabsetPanel(
                       shiny::tabPanel("By Reads", 
                                     fluidRow(
                                       column(12, shiny::plotOutput("reads_by_amplification_plot", width = "100%", height = "600px"))
                                     )),
                       shiny::tabPanel("By Parasitemia", 
                                     fluidRow(
                                       column(12, shiny::plotOutput("parasitemia_plot", width = "100%", height = "600px"))
                                     ))
                     )),
            shiny::tabPanel("Plate Maps",
                     shiny::tabsetPanel(
                       shiny::tabPanel("Layout", 
                                     fluidRow(
                                       column(12, shiny::plotOutput("plate_layout", width = "100%", height = "600px"))
                                     )),
                       shiny::tabPanel("Reads Heatmap", 
                                     fluidRow(
                                       column(12, shiny::plotOutput("reads_heatmap", width = "100%", height = "600px"))
                                     )),
                       shiny::tabPanel("Amplification Heatmap", 
                                     fluidRow(
                                       column(12, shiny::plotOutput("amplification_heatmap", width = "100%", height = "600px"))
                                     ))
                     )),
            shiny::tabPanel("Summary Tables",
                     div(style = "margin: 20px 0;",
                         div(style = "display: inline-block; margin-right: 10px;",
                             selectInput("summary_tables_format", "Download Format:",
                                       choices = c("Excel (.xlsx)" = "excel",
                                                 "CSV Files (.zip)" = "csv"),
                                       selected = "excel",
                                       width = "200px")
                         ),
                         shiny::downloadButton("download_all_tables", "Download All Tables")
                     ),
                     shiny::tabsetPanel(
                       shiny::tabPanel("Reprep/Repool Summary", 
                                     DT::dataTableOutput("reprep_repool_table")),
                       shiny::tabPanel("Polyclonal Information", 
                                     DT::dataTableOutput("polyclonal_table")),
                       shiny::tabPanel("Negative Control Information", 
                                     DT::dataTableOutput("negative_control_table"))
                     )),
            shiny::tabPanel("Filtered Alleles",
                     div(style = "margin: 20px 0;",
                         shiny::downloadButton("download_filtered_alleles", "Download Filtered Alleles (CSV)")
                     ),
                     DT::dataTableOutput("filtered_allele_table"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'

#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ampseq.qc"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
