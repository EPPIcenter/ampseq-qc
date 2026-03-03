#' Server module for managing excluded loci
#'
#' @param id The module id
#' @noRd
mod_excluded_loci_server <- function(id, amplicon_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define default loci
    default_loci <- list(
      species = c('Pf3D7_13_v3-1041593-1041860-1AB',
                 'PmUG01_12_v1-1397996-1398245-1AB',
                 'PocGH01_12_v1-1106456-1106697-1AB',
                 'PvP01_12_v1-1184983-1185208-1AB',
                 'PKNH_12_v2-198869-199113-1AB'),
      long = c('Pf3D7_14_v3-854128-854427-1B',
              'Pf3D7_11_v3-1294284-1294580-1B',
              'Pf3D7_13_v3-1465010-1465307-1B',
              'Pf3D7_13_v3-2841390-2841688-1B2',
              'Pf3D7_13_v3-2844334-2844629-1B2',
              'Pf3D7_02_v3-320652-320949-2',
              'Pf3D7_03_v3-240957-241256-2',
              'Pf3D7_05_v3-615379-615678-2',
              'Pf3D7_06_v3-857454-857753-2')
    )
    
    # Initialize reactive values for loci management
    rv <- reactiveValues(
      available_loci = NULL,
      excluded_loci = list(
        species = default_loci$species,
        long = default_loci$long,
        user = character()
      ),
      pending_changes = FALSE,  # Track if there are pending changes
      current_excluded = c(default_loci$species, default_loci$long)  # Current active excluded loci
    )
    
    # Reactive expression for UI state
    observe({
      if (rv$pending_changes) {
        # Show pending status and enable reprocess button
        shinyjs::show("pending_status")
        shinyjs::enable("reprocess_loci")
      } else {
        # Hide pending status and disable reprocess button
        shinyjs::hide("pending_status")
        shinyjs::disable("reprocess_loci")
      }
    })
    
    # Initialize checkbox groups with default values
    updateCheckboxGroupInput(session, "species_loci",
                           choices = default_loci$species,
                           selected = default_loci$species)
    
    updateCheckboxGroupInput(session, "long_loci",
                           choices = default_loci$long,
                           selected = default_loci$long)
    
    updateCheckboxGroupInput(session, "user_loci",
                           choices = character(),
                           selected = character())
    
    # Update available loci when amplicon data changes
    observe({
      req(amplicon_data())
      rv$available_loci <- unique(amplicon_data()$Locus)
      
      # Update selectize input choices
      updateSelectizeInput(
        session,
        "available_loci",
        choices = setdiff(rv$available_loci, c(rv$excluded_loci$species, rv$excluded_loci$long, rv$excluded_loci$user)),
        server = TRUE
      )
    })
    
    # Handle adding new loci from selectize input
    observeEvent(input$available_loci, {
      if (length(input$available_loci) > 0) {
        # Add selected loci to user-provided list
        rv$excluded_loci$user <- unique(c(rv$excluded_loci$user, input$available_loci))
        rv$pending_changes <- TRUE
        
        # Update user loci checkbox group
        updateCheckboxGroupInput(session, "user_loci",
                               choices = rv$excluded_loci$user,
                               selected = rv$excluded_loci$user)
        
        # Clear the selectize input
        updateSelectizeInput(
          session,
          "available_loci",
          selected = character(0)
        )
      }
    })
    
    # Handle removing loci from checkbox groups
    observeEvent(input$species_loci, {
      rv$excluded_loci$species <- input$species_loci
      rv$pending_changes <- TRUE
    })
    
    observeEvent(input$long_loci, {
      rv$excluded_loci$long <- input$long_loci
      rv$pending_changes <- TRUE
    })
    
    observeEvent(input$user_loci, {
      rv$excluded_loci$user <- input$user_loci
      rv$pending_changes <- TRUE
    })
    
    # Handle reset button
    observeEvent(input$reset_loci, {
      # Reset to default loci
      rv$excluded_loci$species <- default_loci$species
      rv$excluded_loci$long <- default_loci$long
      rv$excluded_loci$user <- character()
      rv$pending_changes <- TRUE
      
      # Update all inputs
      updateCheckboxGroupInput(session, "species_loci",
                             choices = default_loci$species,
                             selected = default_loci$species)
      
      updateCheckboxGroupInput(session, "long_loci",
                             choices = default_loci$long,
                             selected = default_loci$long)
      
      updateCheckboxGroupInput(session, "user_loci",
                             choices = character(),
                             selected = character())
      
      # Update selectize input
      updateSelectizeInput(
        session,
        "available_loci",
        choices = setdiff(rv$available_loci, c(default_loci$species, default_loci$long)),
        selected = character(0)
      )
    })
    
    # Handle reprocess button
    observeEvent(input$reprocess_loci, {
      # Update the current excluded loci list
      rv$current_excluded <- c(rv$excluded_loci$species, rv$excluded_loci$long, rv$excluded_loci$user)
      rv$pending_changes <- FALSE
    })
    
    # Handle collapse panel state
    observeEvent(input$loci_collapse, {
      # Add CSS to rotate chevron when panel is expanded
      if ("loci_panel" %in% input$loci_collapse) {
        shinyjs::runjs("
          document.querySelector('.chevron-icon').style.transform = 'rotate(180deg)';
        ")
      } else {
        shinyjs::runjs("
          document.querySelector('.chevron-icon').style.transform = 'rotate(0deg)';
        ")
      }
    })
    
    # Return the current excluded loci list
    excluded_loci <- reactive({
      rv$current_excluded
    })
    
    return(excluded_loci)
  })
} 