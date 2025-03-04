# R/ui_helpers.R

# Create a UI helper function to display the parasitemia plot UI conditionally
is_field_present <- reactive({
  req(summary_samples())  # Ensure summary_samples is available
  "Parasitemia" %in% colnames(summary_samples())
})