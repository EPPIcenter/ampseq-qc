#' UI module for managing excluded loci
#'
#' @param id The module id
#' @noRd
mod_excluded_loci_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        shinyBS::bsCollapse(
          id = ns("loci_collapse"),
          shinyBS::bsCollapsePanel(
            title = tags$div(
              style = "display: flex; align-items: center;",
              tags$span("Manage Excluded Loci"),
              tags$span(
                id = ns("chevron"),
                icon("chevron-down"),
                style = "margin-left: 10px;"
              )
            ),
            value = "loci_panel",
            style = "primary",
            p("Select loci to exclude from analysis. You can remove any loci, including the default ones."),
            
            # Default loci section
            div(
              style = "margin-bottom: 20px;",
              h5("Default Loci"),
              div(
                style = "margin-left: 10px;",
                h6("Species Loci", style = "color: #666;"),
                checkboxGroupInput(ns("species_loci"), NULL,
                                 choices = NULL,
                                 selected = NULL),
                
                h6("Long Loci", style = "color: #666;"),
                checkboxGroupInput(ns("long_loci"), NULL,
                                 choices = NULL,
                                 selected = NULL)
              )
            ),
            
            # User-provided loci section
            div(
              style = "margin-bottom: 20px;",
              h5("User-Provided Loci"),
              checkboxGroupInput(ns("user_loci"), NULL,
                               choices = NULL,
                               selected = NULL)
            ),
            
            # Add new loci section
            div(
              style = "margin-bottom: 20px;",
              h5("Add More Loci to Exclude"),
              selectizeInput(
                ns("available_loci"),
                NULL,
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select loci to exclude",
                  plugins = list("remove_button", "clear_button")
                )
              )
            ),
            
            # Reset button
            div(
              style = "margin-top: 10px;",
              actionButton(ns("reset_loci"), "Reset to Default Loci", 
                          style = "color: #666; border-color: #666;")
            ),
            # Status indicator and reprocess button
            div(
              style = "margin-top: 10px; display: flex; align-items: center; gap: 10px;",
              # Status indicator
              div(
                id = ns("pending_status"),
                style = "display: none; color: #f0ad4e;",
                icon("exclamation-circle"),
                "Changes pending"
              ),
              # Reprocess button
              div(
                actionButton(ns("reprocess_loci"), "Reprocess Data", 
                           style = "color: #666; border-color: #666;")
              )
            )
          )
        )
      )
    ),
    # Add shinyjs for dynamic updates
    shinyjs::useShinyjs(),
    # Add JavaScript for chevron toggle
    tags$script(sprintf("
      $(document).ready(function() {
        $('#%s').on('show.bs.collapse', function () {
          $('#%s i').removeClass('fa-chevron-down').addClass('fa-chevron-up');
        });
        $('#%s').on('hide.bs.collapse', function () {
          $('#%s i').removeClass('fa-chevron-up').addClass('fa-chevron-down');
        });
      });
    ", ns("loci_collapse"), ns("chevron"), ns("loci_collapse"), ns("chevron")))
  )
} 