test_that("standardise_illumina_sample_name only removes sequencer suffixes", {
  expect_equal(
    standardise_illumina_sample_name(c(
      "s1_S1_L001", "my_sample_S2", "s1_S1_L002", "B1_S01_S3_L001", "Kampala_L001_S5_L001",
      "Site_L123", "sample_A", "ctrl_neg_2", "Pf3D7", NA
    )),
    c("s1", "my_sample", "s1", "B1_S01", "Kampala_L001", "Site_L123", "sample_A", "ctrl_neg_2", "Pf3D7", NA)
  )
})

test_that("names in keep are not trimmed", {
  expect_equal(
    standardise_illumina_sample_name(c("B1_S01", "B1_S01_S1_L001", "B1_S02"), keep = "B1_S01"),
    c("B1_S01", "B1_S01", "B1")
  )
})

test_that("a warning is given when standardising merges different samples", {
  dir <- make_results_dir(withr::local_tempdir())
  allele_path <- file.path(dir, "allele_data.txt")
  alleles <- utils::read.delim(allele_path)
  extra_lane <- alleles[1, ]
  extra_lane$sample_name <- sub("_L001$", "_L002", extra_lane$sample_name)
  write_tsv(rbind(alleles, extra_lane), allele_path)

  expect_warning(read_mad4hatter_results(dir), "made different samples identical")
  expect_no_warning(read_mad4hatter_results(dir, standardise_sample_name = FALSE))
})

test_that("manifest sample names are matched before trimming", {
  dir <- make_results_dir(withr::local_tempdir())
  coverage_path <- file.path(dir, "sample_coverage.txt")
  coverage <- utils::read.delim(coverage_path)
  coverage$sample_name[coverage$sample_name == coverage$sample_name[1]] <- "B1_S01"
  write_tsv(coverage, coverage_path)

  trimmed <- read_mad4hatter_results(dir)
  kept <- read_mad4hatter_results(dir, manifest_sample_names = "B1_S01")

  expect_true("B1" %in% trimmed$sample_coverage$sample_name)
  expect_true("B1_S01" %in% kept$sample_coverage$sample_name)
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

test_that("check_sample_names warns about samples on only one side", {
  manifest <- data.frame(sample_name = c("s1", "s2", "s3"))
  coverage <- data.frame(sample_name = c("s1", "s2", "extra"))

  mismatches <- sample_name_mismatches(manifest, coverage)
  expect_equal(mismatches$not_in_results, "s3")
  expect_equal(mismatches$not_in_manifest, "extra")
  expect_equal(mismatches$n_matched, 2)

  warnings <- character()
  withCallingHandlers(
    check_sample_names(manifest, coverage),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 2)
  expect_match(warnings[1], "in the manifest are not in the Mad4hatter amplicon coverage.*s3")
  expect_match(warnings[2], "not in the manifest.*extra")

  expect_no_warning(check_sample_names(manifest, data.frame(sample_name = c("s1", "s2", "s3"))))
})

test_that("check_sample_names errors when no names match", {
  manifest <- data.frame(sample_name = c("s1", "s2"))
  coverage <- data.frame(sample_name = c("s1_S1_L001", "s2_S2_L001"))

  expect_error(check_sample_names(manifest, coverage), "None of the sample names")
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
