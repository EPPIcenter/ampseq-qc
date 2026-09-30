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
