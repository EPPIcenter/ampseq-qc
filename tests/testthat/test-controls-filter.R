test_that("positive control summary finds multiallelic targets", {
  dir <- make_results_dir(withr::local_tempdir())
  manifest <- read_manifest(write_manifest(file.path(dir, "manifest.csv")))
  allele_data <- add_allele_frequency(read_mad4hatter_results(dir)$allele_data)

  positive <- summarise_positive_controls(allele_data, manifest)

  expect_equal(unique(positive$polyclonal_information$target_name), "targetA")
  expect_equal(positive$category_counts$Count[positive$category_counts$Category == "2 Alleles"], 1)

  strict <- summarise_positive_controls(allele_data, manifest, af_filter = 0.5)
  expect_equal(nrow(strict$polyclonal_information), 0)
})

test_that("negative control target summary uses panel positions", {
  panel <- fixture_panel()
  amplicons_negative <- data.frame(
    sample_name = "neg1",
    target_name = c("targetE", "targetA", "targetD", "targetD"),
    reaction = c("2", "1", "1", "2"),
    reads = c(0, 70, 5, 5),
    OutputPostprocessing = c(0, 60, 0, 0)
  )

  summary <- summarise_negative_control_targets(amplicons_negative, panel, nth_label = 2)

  expect_equal(levels(summary$target_name), c("targetA", "targetD", "targetE"))
  expect_equal(summary$Chromosome[summary$target_name == "targetA"], "chr1")
  expect_equal(summary$Start[summary$target_name == "targetE"], 100)
  expect_equal(summary$Locus_label[summary$target_name == "targetA"], "chr1:100")
  expect_equal(unique(summary$Locus_label[summary$target_name == "targetD"]), "")
  expect_equal(summary$Locus_label[summary$target_name == "targetE"], "chr3:100")
  expect_equal(summary$samples_with_reads[summary$target_name == "targetA"], "neg1")
  expect_true(is.na(summary$samples_with_reads[summary$target_name == "targetE"]))
})

test_that("negative control targets over threshold are not duplicated by shared reactions", {
  amplicons_negative <- data.frame(
    sample_name = "neg1", target_name = c("targetD", "targetD"), reaction = c("1", "2"),
    OutputPostprocessing = c(80, 80)
  )

  expect_equal(nrow(negative_control_targets_over_threshold(amplicons_negative, 50)), 1)
})

test_that("filter_allele_table applies read and allele frequency filters", {
  microhaplotypes <- data.frame(
    sample_name = "s1", gene_id = "g", gene = "dhfr", target_name = "t",
    mhap_aa_positions = "51/59", ref_mhap = "N/C", mhap = c("N/C", "I/R"), reads = c(990, 10)
  )
  group_cols <- allele_table_group_cols()$resmarker_microhaplotype_table

  wsaf <- filter_allele_table(microhaplotypes, group_cols, read_filter = 0, af_filter = 0.05, filter_wsaf = TRUE)
  reads_only <- filter_allele_table(microhaplotypes, group_cols, read_filter = 0, af_filter = 0.05, filter_wsaf = FALSE)

  expect_equal(wsaf$mhap, "N/C")
  expect_equal(reads_only$mhap, c("N/C", "I/R"))
  expect_equal(names(reads_only), names(microhaplotypes))
})

test_that("missing reports list absent samples and targets", {
  dir <- make_results_dir(withr::local_tempdir())
  manifest <- read_manifest(write_manifest(file.path(dir, "manifest.csv")))
  results <- read_mad4hatter_results(dir)

  expect_equal(missing_samples_report(results$allele_data, manifest)$sample_name, "s3")
  expect_false("targetB" %in% missing_targets_report(results$panel_information, results$allele_data)$target_name)
})
