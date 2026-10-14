fixture_panel <- function() {
  data.frame(
    target_name = c("targetA", "targetB", "targetC", "targetD", "targetE", "targetLong", "PmUG01_12_v1-1398020-1398213"),
    chrom = c("chr1", "chr1", "chr2", "chr2", "chr3", "chr3", "PmUG01_12_v1"),
    insert_start = c(100, 300, 100, 500, 100, 700, 1398020),
    insert_end = c(200, 400, 200, 600, 200, 1000, 1398213),
    fwd_primer = strrep("A", 20),
    rev_primer = strrep("T", 20),
    pool = c("D1.1", "D1.1", "R1.2", "R1.2,R2.1", "R2.1", "R2.1", "R1.2")
  )
}

fixture_manifest <- function() {
  data.frame(
    sample_name = c("s1", "s2", "PC1", "neg1", "s3", "neg2"),
    SampleType = c("sample", "sample", "positive", "negative", "sample", "negative"),
    Batch = "batch1",
    Column = 1:6,
    Row = c("a", "a", "b", "b", "c", "c"),
    Parasitemia = c(1000, 10, 5000, 0, 100, 0)
  )
}

fixture_target_reads <- function() {
  targets <- fixture_panel()$target_name
  list(
    s1 = stats::setNames(rep(500, length(targets)), targets),
    s2 = stats::setNames(c(500, rep(0, length(targets) - 1)), targets),
    PC1 = stats::setNames(rep(500, length(targets)), targets),
    neg1 = stats::setNames(c(60, rep(0, length(targets) - 1)), targets)
  )
}

write_tsv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(df, path, sep = "\t", row.names = FALSE, quote = FALSE)
}

raw_name <- function(sample, i) paste0(sample, "_S", i, "_L001")

make_results_dir <- function(dir, resmarkers = TRUE, collapsed = TRUE) {
  target_reads <- fixture_target_reads()
  samples <- names(target_reads)

  amplicon_coverage <- do.call(rbind, lapply(seq_along(samples), function(i) {
    reads <- target_reads[[samples[i]]]
    data.frame(
      sample_name = raw_name(samples[i], i),
      target_name = names(reads),
      reads = reads + 10,
      OutputDada2 = reads,
      OutputPostprocessing = reads
    )
  }))

  sample_coverage <- do.call(rbind, lapply(seq_along(samples), function(i) {
    total <- sum(target_reads[[samples[i]]])
    data.frame(
      sample_name = raw_name(samples[i], i),
      stage = c("Input", "No Dimers", "Amplicons", "OutputDada2", "OutputPostprocessing"),
      reads = c(total * 2, total * 1.5, total * 1.2, total, total)
    )
  }))

  allele_data <- do.call(rbind, lapply(seq_along(samples), function(i) {
    reads <- target_reads[[samples[i]]]
    reads <- reads[reads > 0]
    data.frame(
      sample_name = raw_name(samples[i], i),
      target_name = names(reads),
      asv = "ACGT",
      pseudocigar_unmasked = "1A",
      asv_masked = "ACGT",
      pseudocigar_masked = "1A",
      reads = reads,
      pool = "D1.1"
    )
  }))
  # A second allele at targetA in the positive control
  allele_data <- rbind(allele_data, data.frame(
    sample_name = raw_name("PC1", 3), target_name = "targetA", asv = "ACGA",
    pseudocigar_unmasked = "4A", asv_masked = "ACGA", pseudocigar_masked = "4A",
    reads = 100, pool = "D1.1"
  ))

  write_tsv(amplicon_coverage, file.path(dir, "amplicon_coverage.txt"))
  write_tsv(sample_coverage, file.path(dir, "sample_coverage.txt"))
  write_tsv(allele_data, file.path(dir, "allele_data.txt"))
  write_tsv(fixture_panel(), file.path(dir, "panel_information", "amplicon_info.tsv"))

  if (collapsed) {
    write_tsv(allele_data[, c("sample_name", "target_name", "asv_masked", "pseudocigar_masked", "reads", "pool")],
              file.path(dir, "allele_data_collapsed.txt"))
  }
  if (resmarkers) {
    write_tsv(
      data.frame(
        sample_name = raw_name("s1", 1), gene_id = "PF3D7_0417200", gene = "dhfr",
        aa_position = 51, ref_codon = "AAT", alt_codon = c("AAT", "ATT"),
        reads = c(990, 10)
      ),
      file.path(dir, "resistance_marker_module", "resmarker_table.txt")
    )
    write_tsv(
      data.frame(
        sample_name = raw_name("s1", 1), gene_id = "PF3D7_0417200", gene = "dhfr",
        target_name = "targetC", mhap_aa_positions = "51/59", ref_mhap = "N/C",
        mhap = c("N/C", "I/R"), reads = c(990, 10)
      ),
      file.path(dir, "resistance_marker_module", "resmarker_microhaplotype_table.txt")
    )
  }
  invisible(dir)
}

write_manifest <- function(path, sep = ",") {
  utils::write.table(fixture_manifest(), path, sep = sep, row.names = FALSE, quote = FALSE)
  invisible(path)
}
