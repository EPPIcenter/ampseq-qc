#' Styled HTML table
#'
#' @param df A data frame.
#' @param ... Passed to [knitr::kable()].
#' @return A `kableExtra` table.
#' @export
styled_table <- function(df, ...) {
  knitr::kable(df, ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}

#' Interactive version of a ggplot
#'
#' @param p A ggplot with a `text` aesthetic.
#' @param ... Passed to [plotly::ggplotly()].
#' @return A plotly object.
#' @export
interactive_plot <- function(p, ...) {
  plotly::ggplotly(p, tooltip = "text", ...)
}

#' Print plots as a Quarto tabset
#'
#' Must be called from a chunk with `results: asis`.
#'
#' @param plots Named list of ggplots; names are used as tab titles.
#' @return `NULL`, invisibly.
#' @export
create_tabset <- function(plots) {
  cat("\n\n::: {.panel-tabset}\n\n")
  purrr::iwalk(plots, ~ {
    cat("## ", .y, "\n\n")
    print(.x)
    cat("\n\n")
  })
  cat(":::\n")
  invisible(NULL)
}

#' Print a plot at a given size
#'
#' Knits the plot as a child chunk so its size can differ from the calling
#' chunk. Must be called from a chunk with `results: asis`.
#'
#' @param plot A ggplot.
#' @param fig_width,fig_height Figure size in inches.
#' @return `NULL`, invisibly.
#' @export
print_sized_plot <- function(plot, fig_width, fig_height) {
  env <- new.env()
  env$plot <- plot
  chunk <- sprintf("```{r, fig.width=%s, fig.height=%s, echo=FALSE}\nprint(plot)\n```", fig_width, fig_height)
  cat(knitr::knit_child(text = chunk, envir = env, quiet = TRUE))
  invisible(NULL)
}

#' Path to the QC report template
#'
#' Copy this file if you want to customise the report.
#'
#' @return Path to the Quarto template installed with the package.
#' @export
qc_report_template <- function() {
  system.file("quarto", "QC_report.qmd", package = "ampseqQC", mustWork = TRUE)
}

#' Render the QC report
#'
#' Renders the QC report to HTML with the Quarto CLI and writes the QC output
#' tables (repool/reprep summaries, control summaries, filtered allele tables
#' and missing sample/target reports) to `output_dir`.
#'
#' @param results_dir Mad4hatter results directory.
#' @param manifest_file Sample manifest, see [read_manifest()].
#' @param output_dir Directory for the report and output tables.
#' @param output_file File name of the HTML report.
#' @param panel Panel settings, see [panel_settings()].
#' @param standardise_sample_name If `TRUE`, sample names are trimmed to match
#'   the manifest, see [standardise_sample_names()].
#' @param read_threshold Reads needed to count a target as successfully
#'   amplified.
#' @param read_filter Minimum reads per allele, for the positive control plot
#'   and the filtered allele tables.
#' @param af_filter Minimum within-sample allele frequency, for the positive
#'   control plot and (if `filter_wsaf_final_allele_table`) the filtered allele
#'   tables.
#' @param negative_control_read_threshold Reads above which a negative control
#'   target is reported.
#' @param reprep_threshold Proportion of targets amplified below which a
#'   sample needs re-prep.
#' @param repool_threshold Proportion of targets amplified below which a
#'   sample needs re-pool.
#' @param allele_col Allele ID column, `"asv"` or `"pseudocigar_masked"`.
#' @param long_target_threshold Targets longer than this (bp, including
#'   primers) are excluded from QC.
#' @param filter_wsaf_final_allele_table If `FALSE`, only `read_filter` is
#'   applied to the filtered allele tables.
#' @param quarto Path to the Quarto CLI.
#'
#' @return Path to the rendered report, invisibly.
#' @export
render_qc_report <- function(results_dir,
                             manifest_file,
                             output_dir = ".",
                             output_file = "QC_report.html",
                             panel = default_panel_settings(),
                             standardise_sample_name = TRUE,
                             read_threshold = 100,
                             read_filter = 0,
                             af_filter = 0.01,
                             negative_control_read_threshold = 50,
                             reprep_threshold = 0.5,
                             repool_threshold = 0.75,
                             allele_col = "pseudocigar_masked",
                             long_target_threshold = 275,
                             filter_wsaf_final_allele_table = TRUE,
                             quarto = Sys.which("quarto")) {
  if (!nzchar(quarto)) {
    stop("The Quarto CLI was not found. Install it from https://quarto.org or pass its path with `quarto`.", call. = FALSE)
  }
  if (!inherits(panel, "ampseqQC_panel_settings")) {
    stop("`panel` must be created with panel_settings() or default_panel_settings().", call. = FALSE)
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  work_dir <- tempfile("ampseqQC_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  file.copy(qc_report_template(), work_dir)

  panel_settings_file <- file.path(work_dir, "panel_settings.rds")
  saveRDS(panel, panel_settings_file)

  params <- list(
    results_dir = normalizePath(results_dir, mustWork = TRUE),
    manifest_file = normalizePath(manifest_file, mustWork = TRUE),
    output_dir = output_dir,
    panel_settings_file = panel_settings_file,
    standardise_sample_name = standardise_sample_name,
    read_threshold = read_threshold,
    read_filter = read_filter,
    af_filter = af_filter,
    negative_control_read_threshold = negative_control_read_threshold,
    reprep_threshold = reprep_threshold,
    repool_threshold = repool_threshold,
    allele_col = allele_col,
    long_target_threshold = long_target_threshold,
    filter_wsaf_final_allele_table = filter_wsaf_final_allele_table
  )
  params_file <- file.path(work_dir, "params.yml")
  writeLines(paste0(names(params), ": ", vapply(params, yaml_scalar, character(1))), params_file)

  old_wd <- setwd(work_dir)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  status <- system2(quarto, c(
    "render", "QC_report.qmd",
    "--execute-params", "params.yml",
    "--output", shQuote(output_file)
  ))
  if (status != 0) {
    stop("Quarto failed to render the QC report.", call. = FALSE)
  }

  report_path <- file.path(output_dir, output_file)
  file.copy(file.path(work_dir, output_file), report_path, overwrite = TRUE)
  message("Saved report: ", report_path)
  invisible(report_path)
}

yaml_scalar <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    stop("Report parameters must be single, non-missing values.", call. = FALSE)
  }
  if (is.logical(x)) {
    return(tolower(as.character(x)))
  }
  if (is.numeric(x)) {
    return(format(x, scientific = FALSE))
  }
  paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)), '"')
}
