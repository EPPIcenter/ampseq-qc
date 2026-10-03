qc_fixture <- function() {
  dir <- make_results_dir(withr::local_tempdir(.local_envir = parent.frame()))
  manifest <- read_manifest(write_manifest(file.path(dir, "manifest.csv")))
  results <- read_mad4hatter_results(dir)
  panel_information <- results$panel_information

  excluded <- qc_excluded_targets(panel_information)
  panel_reactions <- assign_reactions(panel_information) |>
    dplyr::filter(!target_name %in% excluded$excluded)
  amplicon_coverage_qc <- results$amplicon_coverage[!results$amplicon_coverage$target_name %in% excluded$excluded, ]
  nloci_table <- count_targets_per_reaction(panel_reactions)
  amplicon_coverage_with_manifest <- merge_amplicon_coverage(amplicon_coverage_qc, manifest, panel_reactions)
  summary_samples <- summarise_samples(amplicon_coverage_with_manifest, nloci_table, manifest, 100)

  list(
    manifest = manifest, results = results, excluded = excluded, nloci_table = nloci_table,
    amplicon_coverage_with_manifest = amplicon_coverage_with_manifest, summary_samples = summary_samples
  )
}

test_that("qc_excluded_targets combines panel exclusions and long targets", {
  excluded <- qc_excluded_targets(fixture_panel(), long_target_threshold = 275)

  expect_equal(excluded$long, "targetLong")
  expect_true(all(c("targetLong", "PmUG01_12_v1-1398020-1398213") %in% excluded$excluded))
  expect_length(qc_excluded_targets(fixture_panel(), long_target_threshold = 1000)$long, 0)
})

test_that("shared targets count towards each reaction", {
  fixture <- qc_fixture()

  nloci <- stats::setNames(fixture$nloci_table$nreactionloci, fixture$nloci_table$reaction)
  expect_equal(nloci[["1"]], 4)
  expect_equal(nloci[["2"]], 2)
})

test_that("reads per sample are not double counted for shared targets", {
  fixture <- qc_fixture()

  s1 <- fixture$summary_samples[fixture$summary_samples$sample_name == "s1", ]
  expect_equal(unique(s1$reads_per_sample), 500 * 5)
  expect_equal(sum(s1$reads_per_reaction), 500 * 6)
  expect_equal(s1$prop_good_loci, c(1, 1))
})

test_that("samples are classified per reaction", {
  fixture <- qc_fixture()

  summary <- generate_reprep_repool_table(fixture$summary_samples, 0.5, 0.75)

  expect_false("neg1" %in% summary$sample_name)
  expect_equal(summary$status[summary$sample_name == "s2"], c("reprep", "reprep"))
  expect_equal(summary$status[summary$sample_name == "s1"], c("pass", "pass"))
  expect_true("SampleType" %in% names(summary))
})

test_that("fill_missing_data adds missing samples but not negative controls", {
  fixture <- qc_fixture()
  summary <- generate_reprep_repool_table(fixture$summary_samples, 0.5, 0.75)

  filled <- fill_missing_data(summary, fixture$manifest)

  s3 <- filled[filled$sample_name == "s3", ]
  expect_equal(nrow(s3), 2)
  expect_equal(unique(s3$reason), "no data for sample")
  expect_equal(unique(s3$SampleType), "sample")
  expect_false("neg2" %in% filled$sample_name)
})

test_that("controls are identified by SampleType, not sample name", {
  fixture <- qc_fixture()
  summary <- generate_reprep_repool_table(fixture$summary_samples, 0.5, 0.75) |>
    fill_missing_data(fixture$manifest)

  counts <- qc_status_counts(summary)
  expect_equal(counts$pass, 1)
  expect_equal(counts$reprep, 2)
  expect_equal(counts$total, 3)

  by_batch <- qc_summary_table(summary)
  expect_equal(by_batch$total, 3)
  expect_equal(by_batch$pass_rate, round(100 / 3, 1))
})

test_that("QC summaries count each sample once by its worst reaction status", {
  summary <- tibble::tibble(
    sample_name = c("a", "a", "b", "b", "c", "c", "d", "d"),
    Batch = "B1",
    SampleType = "sample",
    reaction = rep(c("1", "2"), 4),
    status = c("pass", "pass", "pass", "reprep", "repool", "pass", "reprep", "reprep")
  )

  status <- sample_qc_status(summary)
  expect_equal(status$status[order(status$sample_name)], c("pass", "reprep", "repool", "reprep"))

  counts <- qc_status_counts(summary)
  expect_equal(c(counts$pass, counts$repool, counts$reprep, counts$total), c(1, 1, 2, 4))

  by_batch <- qc_summary_table(summary)
  expect_equal(by_batch$total, 4)
  expect_equal(sum(qc_summary_by_batch(summary)$percentage), 100)
})

test_that("samples_with_status returns rows for one status", {
  fixture <- qc_fixture()
  summary <- generate_reprep_repool_table(fixture$summary_samples, 0.5, 0.75) |>
    fill_missing_data(fixture$manifest)

  expect_setequal(samples_with_status(summary, "reprep")$sample_name, c("s2", "s3"))
  expect_equal(nrow(samples_with_status(summary, "repool")), 0)
})
