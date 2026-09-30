test_that("standardise_sample_names trims sequencing information", {
  expect_equal(standardise_sample_names(c("s1_S1_L001", "my_sample_S2")), c("s1", "my_sample"))
})

test_that("read_manifest detects the separator from the manifest", {
  dir <- withr::local_tempdir()

  comma <- read_manifest(write_manifest(file.path(dir, "comma.csv")))
  semicolon <- read_manifest(write_manifest(file.path(dir, "semicolon.csv"), sep = ";"))

  expect_equal(comma, semicolon)
  expect_equal(unique(comma$Row), c("A", "B", "C"))
})

test_that("read_manifest checks required columns", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(fixture_manifest()[, -6], path, row.names = FALSE)

  expect_error(read_manifest(path), "missing required column\\(s\\): Parasitemia")
})

test_that("read_mad4hatter_results standardises sample names", {
  dir <- make_results_dir(withr::local_tempdir())

  results <- read_mad4hatter_results(dir)
  raw <- read_mad4hatter_results(dir, standardise_sample_name = FALSE)

  expect_setequal(unique(results$allele_data$sample_name), c("s1", "s2", "PC1", "neg1"))
  expect_setequal(unique(results$collapsed_allele_data$sample_name), c("s1", "s2", "PC1", "neg1"))
  expect_true(all(grepl("_L001$", raw$allele_data$sample_name)))
  expect_true(all(grepl("_L001$", raw$collapsed_allele_data$sample_name)))
})

test_that("optional outputs are NULL when absent", {
  dir <- make_results_dir(withr::local_tempdir(), resmarkers = FALSE, collapsed = FALSE)

  results <- read_mad4hatter_results(dir)

  expect_null(results$collapsed_allele_data)
  expect_null(results$resmarker_table)
  expect_null(results$resmarker_microhaplotype_table)
  expect_true(all(c("resmarker_table", "resmarker_microhaplotype_table") %in% names(results)))
})

test_that("missing required outputs are reported", {
  dir <- make_results_dir(withr::local_tempdir())
  file.remove(file.path(dir, "allele_data.txt"))

  expect_error(read_mad4hatter_results(dir), "allele_data.txt")
})
