# Simulates the tutorial dataset in inst/extdata/tutorial: a Mad4hatter run of
# the MAD4HatTeR D1, R1 and R2 pools (D1.1, R1.2 and R2.1) on two 96-well
# plates, with a matching manifest. Run from the package root:
#   Rscript data-raw/simulate_tutorial_data.R
#
# Built in scenarios:
#   * the Batch2 plate has no samples in column 12
#   * parasitemia from 1 to 100,000 p/uL, so low density samples fail QC
#   * Batch2 is sequenced less deeply than Batch1
#   * B2_S05 and B2_S06 have a failed reaction 2
#   * B1_S07 is a P. vivax co-infection
#   * B2_S20 is in the manifest but was not sequenced
#   * B2_NEG1 has low level contamination
#   * positive controls are monoclonal 3D7 with a few low frequency PCR errors

library(dplyr)

set.seed(20260929)

mad4hatter_sha <- "bbd31ccbdb75cdf846c001781c7be74420ef0cd0"
pools <- c(D1 = "D1.1", R1 = "R1.2", R2 = "R2.1")
pool_reactions <- c(D1 = 1, R1 = 1, R2 = 2)
out_dir <- file.path("inst", "extdata", "tutorial")
results_dir <- file.path(out_dir, "results")

# Panel information, combined as Mad4hatter's build_amplicon_info.py does ----

read_pool <- function(pool) {
  url <- sprintf(
    "https://raw.githubusercontent.com/EPPIcenter/mad4hatter/%s/panel_information/%s/%s_amplicon_info.tsv",
    mad4hatter_sha, pools[[pool]], pools[[pool]]
  )
  read.delim(url) |> mutate(pool = pool)
}

panel_information <- bind_rows(lapply(names(pools), read_pool)) |>
  group_by(target_name, chrom, insert_start, insert_end, fwd_primer, rev_primer) |>
  summarise(pool = paste(pool, collapse = ","), .groups = "drop") |>
  arrange(target_name)

# Target properties ----

random_sequence <- function(n) paste(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")

mutate_sequence <- function(sequence, n_snps) {
  bases <- strsplit(sequence, "")[[1]]
  positions <- sort(sample(seq_along(bases), n_snps))
  for (position in positions) {
    bases[position] <- sample(setdiff(c("A", "C", "G", "T"), bases[position]), 1)
  }
  list(
    sequence = paste(bases, collapse = ""),
    pseudocigar = paste0(positions, bases[positions], collapse = "")
  )
}

# ASVs are short placeholder sequences to keep the dataset small; the QC does
# not use the sequences themselves.
make_haplotypes <- function(n_haplotypes) {
  reference <- random_sequence(30)
  haplotypes <- data.frame(haplotype = 1, asv = reference, pseudocigar = ".")
  for (h in seq_len(n_haplotypes - 1)) {
    variant <- mutate_sequence(reference, sample(1:4, 1))
    haplotypes <- rbind(haplotypes, data.frame(haplotype = h + 1, asv = variant$sequence, pseudocigar = variant$pseudocigar))
  }
  haplotypes
}

targets <- panel_information |>
  mutate(
    target_length = insert_end - insert_start + nchar(fwd_primer) + nchar(rev_primer),
    species = case_when(
      startsWith(chrom, "PvP01") ~ "Pv",
      startsWith(chrom, "PmUG01") ~ "Pm",
      startsWith(chrom, "PocGH01") ~ "Po",
      startsWith(chrom, "PKNH") ~ "Pk",
      TRUE ~ "Pf"
    ),
    efficiency = rlnorm(n(), 0, 0.6) * ifelse(target_length > 275, 0.3, 1),
    n_haplotypes = ifelse(grepl("D1", pool), sample(2:10, n(), replace = TRUE), sample(1:3, n(), replace = TRUE))
  )
target_pools <- strsplit(targets$pool, ",")
haplotypes <- lapply(targets$n_haplotypes, make_haplotypes)
names(haplotypes) <- targets$target_name

# Manifest ----

# Controls go in rows E-H of the last filled column
make_batch <- function(batch, prefix, n_columns = 12) {
  wells <- expand.grid(Row = LETTERS[1:8], Column = seq_len(n_columns), stringsAsFactors = FALSE)
  wells$SampleType <- "sample"
  wells$SampleType[wells$Column == n_columns & wells$Row %in% c("G", "H")] <- "positive"
  wells$SampleType[wells$Column == n_columns & wells$Row %in% c("E", "F")] <- "negative"
  wells$sample_name <- NA_character_
  for (type in c("sample", "positive", "negative")) {
    is_type <- wells$SampleType == type
    label <- c(sample = "S", positive = "POS", negative = "NEG")[[type]]
    width <- if (type == "sample") 2 else 1
    wells$sample_name[is_type] <- sprintf(paste0("%s_%s%0", width, "d"), prefix, label, seq_len(sum(is_type)))
  }
  wells$Batch <- batch
  wells$Parasitemia <- ifelse(
    wells$SampleType == "sample", round(10^runif(nrow(wells), 0, 5), 1),
    ifelse(wells$SampleType == "positive", 10000, 0)
  )
  wells
}

manifest <- bind_rows(make_batch("Batch1", "B1"), make_batch("Batch2", "B2", n_columns = 11)) |>
  select(sample_name, SampleType, Batch, Column, Row, Parasitemia)

# Scenario samples have high parasitemia so their failures come from the scenario
manifest$Parasitemia[manifest$sample_name == "B1_S07"] <- 8500.0
manifest$Parasitemia[manifest$sample_name == "B2_S05"] <- 21340.5
manifest$Parasitemia[manifest$sample_name == "B2_S06"] <- 12876.2

# Reads ----

simulate_sample <- function(sample) {
  batch_depth <- if (sample$Batch == "Batch2") 0.6 else 1
  depth <- if (sample$SampleType == "negative") {
    0
  } else {
    1200 * sample$Parasitemia / (sample$Parasitemia + 50) * rlnorm(1, 0, 0.4) * batch_depth
  }

  reaction_multiplier <- c(`1` = 1, `2` = 1)
  if (sample$sample_name %in% c("B2_S05", "B2_S06")) reaction_multiplier[["2"]] <- 0.02

  target_multiplier <- vapply(target_pools, function(p) mean(reaction_multiplier[as.character(pool_reactions[p])]), numeric(1))
  species_multiplier <- ifelse(targets$species == "Pf", 1, 0)
  if (sample$sample_name == "B1_S07") species_multiplier[targets$species == "Pv"] <- 0.5

  mu <- depth * targets$efficiency * target_multiplier * species_multiplier
  target_reads <- ifelse(mu > 0, rnbinom(nrow(targets), mu = mu, size = 8), 0)

  if (sample$sample_name == "B2_NEG1") {
    contaminated <- sample(which(grepl("D1", targets$pool) & targets$species == "Pf"), 20)
    target_reads[contaminated] <- sample(5:60, 20, replace = TRUE)
  }

  # Strains and their proportions
  coi <- if (sample$SampleType == "sample") sample(1:3, 1, prob = c(0.6, 0.3, 0.1)) else 1
  proportions <- rgamma(coi, 2)
  proportions <- proportions / sum(proportions)

  alleles <- lapply(which(target_reads > 0), function(i) {
    target_haplotypes <- haplotypes[[i]]
    strain_haplotypes <- if (sample$SampleType == "positive") {
      rep(1, coi)
    } else {
      weights <- rev(seq_len(nrow(target_haplotypes)))
      sample(target_haplotypes$haplotype, coi, replace = TRUE, prob = weights)
    }
    reads <- as.vector(rmultinom(1, target_reads[i], proportions))
    allele_reads <- tapply(reads, strain_haplotypes, sum)
    allele <- data.frame(
      target_name = targets$target_name[i],
      haplotype = as.integer(names(allele_reads)),
      reads = as.vector(allele_reads)
    )
    # Occasional low frequency PCR error allele
    if (target_reads[i] > 200 && runif(1) < 0.01) {
      error_reads <- max(1, round(target_reads[i] * runif(1, 0.02, 0.04)))
      allele$reads[1] <- allele$reads[1] - error_reads
      allele <- rbind(allele, data.frame(target_name = targets$target_name[i], haplotype = -1, reads = error_reads))
    }
    allele[allele$reads > 0, ]
  })
  alleles <- bind_rows(alleles)

  list(target_reads = target_reads, alleles = alleles, depth = depth)
}

sequenced <- manifest |>
  filter(sample_name != "B2_S20")

sample_results <- lapply(seq_len(nrow(sequenced)), function(i) simulate_sample(sequenced[i, ]))

raw_sample_names <- sprintf("%s_S%d_L001", sequenced$sample_name, seq_len(nrow(sequenced)))

amplicon_coverage <- bind_rows(lapply(seq_along(sample_results), function(i) {
  post <- sample_results[[i]]$target_reads
  data.frame(
    sample_name = raw_sample_names[i],
    target_name = targets$target_name,
    reads = round(post * runif(length(post), 1.05, 1.25)) + rpois(length(post), 1),
    OutputDada2 = post + round(post * runif(length(post), 0, 0.03)),
    OutputPostprocessing = post
  )
}))

sample_coverage <- bind_rows(lapply(seq_along(sample_results), function(i) {
  coverage <- amplicon_coverage[amplicon_coverage$sample_name == raw_sample_names[i], ]
  amplicons <- sum(coverage$reads)
  no_dimers <- round(amplicons * runif(1, 1.01, 1.05)) + rpois(1, 20)
  dimer_fraction <- min(0.97, 0.05 + 0.85 * exp(-sample_results[[i]]$depth / 150) + runif(1, 0, 0.05))
  data.frame(
    sample_name = raw_sample_names[i],
    stage = c("Input", "No Dimers", "Amplicons", "OutputDada2", "OutputPostprocessing"),
    reads = c(
      round(no_dimers / (1 - dimer_fraction)),
      no_dimers,
      amplicons,
      sum(coverage$OutputDada2),
      sum(coverage$OutputPostprocessing)
    )
  )
}))

allele_data <- bind_rows(lapply(seq_along(sample_results), function(i) {
  alleles <- sample_results[[i]]$alleles
  if (nrow(alleles) == 0) {
    return(NULL)
  }
  alleles$sample_name <- raw_sample_names[i]
  alleles
})) |>
  left_join(targets |> select(target_name, pool), by = "target_name")

allele_sequences <- bind_rows(lapply(names(haplotypes), function(target) {
  cbind(target_name = target, haplotypes[[target]])
}))
error_alleles <- allele_data |>
  filter(haplotype == -1) |>
  distinct(target_name) |>
  left_join(allele_sequences |> filter(haplotype == 1), by = "target_name") |>
  rowwise() |>
  mutate(variant = list(mutate_sequence(asv, 1))) |>
  mutate(haplotype = -1, asv = variant$sequence, pseudocigar = variant$pseudocigar) |>
  ungroup() |>
  select(target_name, haplotype, asv, pseudocigar)
allele_sequences <- bind_rows(allele_sequences, error_alleles)

allele_data <- allele_data |>
  left_join(allele_sequences, by = c("target_name", "haplotype")) |>
  transmute(
    sample_name,
    target_name,
    asv,
    pseudocigar_unmasked = pseudocigar,
    asv_masked = asv,
    pseudocigar_masked = pseudocigar,
    reads,
    pool
  ) |>
  arrange(sample_name, target_name, desc(reads))

# Write ----

write_tsv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(df, path, sep = "\t", row.names = FALSE, quote = FALSE)
}

unlink(out_dir, recursive = TRUE)
write_tsv(panel_information, file.path(results_dir, "panel_information", "amplicon_info.tsv"))
write_tsv(sample_coverage, file.path(results_dir, "sample_coverage.txt"))
write_tsv(amplicon_coverage, file.path(results_dir, "amplicon_coverage.txt"))
write_tsv(allele_data, file.path(results_dir, "allele_data.txt"))
write.csv(manifest, file.path(out_dir, "manifest.csv"), row.names = FALSE, quote = FALSE)
