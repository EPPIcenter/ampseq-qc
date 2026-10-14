test_that("QC status plot keeps the pass bar when percentages sum to just over 100", {
  qc_summary <- tibble::tibble(
    Batch = "Batch1",
    status = c("pass", "repool", "reprep"),
    count = c(96, 10, 3)
  ) %>%
    dplyr::mutate(total = sum(count), percentage = 100 * count / total)

  bars <- ggplot2::ggplot_build(plot_qc_status_by_batch(qc_summary))$data[[1]]

  expect_equal(nrow(bars), 3)
  expect_false(anyNA(bars$ymax))
})

test_that("negative control histogram shows the read threshold as a dashed line", {
  negatives <- data.frame(
    negative = "Batch1 Well: A1",
    reaction = c("1", "2"),
    OutputPostprocessing = c(5, 20)
  )
  is_vline <- function(p) vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), logical(1))

  without <- plot_negative_control_histogram(negatives)
  with_filter <- plot_negative_control_histogram(negatives, negative_control_read_threshold = 100)

  expect_false(any(is_vline(without)))
  vline <- with_filter$layers[is_vline(with_filter)][[1]]
  expect_equal(vline$data$xintercept, 100)
  expect_equal(vline$aes_params$linetype, "dashed")
  expect_equal(with_filter$coordinates$limits$x, c(0, 110))
})
