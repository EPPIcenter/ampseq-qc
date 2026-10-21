#' Panel settings
#'
#' Panel settings describe how the pools in a Mad4hatter run map onto
#' multiplex PCR reactions, and which targets should be left out of QC
#' calculations (they are still kept in the filtered allele outputs).
#'
#' The pools present in a run are always taken from the `pool` column of
#' `panel_information/amplicon_info.tsv`; the settings only need to know which
#' reaction each pool was amplified in. Pools can be supplied in any
#' combination or subset.
#'
#' @param pool_reactions Named character vector mapping pool names (as passed
#'   to Mad4hatter's `--pools`) to reaction labels, e.g.
#'   `c(D1 = "1", R1 = "1", R2 = "2")`.
#' @param excluded_targets Character vector of target names to exclude from QC
#'   calculations.
#' @param base Optional panel settings to extend. Pools in `pool_reactions`
#'   override those in `base`, and `excluded_targets` are added to those in
#'   `base`.
#'
#' @return An object of class `ampseqQC_panel_settings`.
#' @export
#' @examples
#' # Run R1.2 in its own reaction instead of alongside D1
#' panel_settings(c(R1 = "3", R1.2 = "3"), base = default_panel_settings())
#'
#' # A bespoke panel
#' panel_settings(c(poolA = "A", poolB = "B"))
panel_settings <- function(pool_reactions = character(), excluded_targets = character(), base = NULL) {
  pool_reactions <- validate_pool_reactions(pool_reactions)
  excluded_targets <- as.character(excluded_targets)

  if (!is.null(base)) {
    if (!inherits(base, "ampseqQC_panel_settings")) {
      stop("`base` must be created with panel_settings() or default_panel_settings().", call. = FALSE)
    }
    merged <- base$pool_reactions
    merged[names(pool_reactions)] <- pool_reactions
    pool_reactions <- merged
    excluded_targets <- c(base$excluded_targets, excluded_targets)
  }

  structure(
    list(pool_reactions = pool_reactions, excluded_targets = unique(excluded_targets)),
    class = "ampseqQC_panel_settings"
  )
}

validate_pool_reactions <- function(pool_reactions) {
  if (length(pool_reactions) == 0) {
    return(stats::setNames(character(), character()))
  }
  pools <- names(pool_reactions)
  if (is.null(pools) || any(is.na(pools) | pools == "")) {
    stop("`pool_reactions` must be a named vector, e.g. c(D1 = \"1\", R2 = \"2\").", call. = FALSE)
  }
  if (anyDuplicated(pools)) {
    stop("Duplicate pools in `pool_reactions`: ", paste(unique(pools[duplicated(pools)]), collapse = ", "), call. = FALSE)
  }
  if (any(is.na(pool_reactions))) {
    stop("Every pool in `pool_reactions` must have a reaction.", call. = FALSE)
  }
  stats::setNames(as.character(pool_reactions), pools)
}

#' Default panel settings for MAD4HatTeR and PfPHAST
#'
#' Covers the current, versioned and legacy pool names supported by Mad4hatter
#' for the MAD4HatTeR (D1, R1, R2) and PfPHAST (M1, M1.addon, M2) panels, using
#' the recommended two-reaction layout: reaction 1 contains D1 and R1 (or M1
#' and M1.addon), reaction 2 contains R2 (or M2).
#'
#' Targets excluded from QC are the ldh targets for *P. falciparum* and
#' non-falciparum species (D1.1, R1.1, R1.2, M1.1, including their legacy
#' names) and the non-falciparum mitochondrial targets in M1.addon.
#'
#' @return An object of class `ampseqQC_panel_settings`.
#' @export
#' @examples
#' default_panel_settings()
default_panel_settings <- function() {
  panel_settings(
    pool_reactions = c(
      # MAD4HatTeR
      "D1" = "1", "D1.1" = "1", "1A" = "1",
      "R1" = "1", "R1.1" = "1", "R1.2" = "1", "1B" = "1", "5" = "1",
      "R2" = "2", "R2.1" = "2", "2" = "2",
      # PfPHAST
      "M1" = "1", "M1.1" = "1", "M1.addon" = "1",
      "M2" = "2", "M2.1" = "2"
    ),
    excluded_targets = c(
      # ldh targets, legacy names
      "Pf3D7_13_v3-1041593-1041860-1AB",
      "PmUG01_12_v1-1397996-1398245-1AB",
      "PocGH01_12_v1-1106456-1106697-1AB",
      "PvP01_12_v1-1184983-1185208-1AB",
      "PKNH_12_v2-198869-199113-1AB",
      # ldh targets
      "Pf3D7_13_v3-1041624-1041829",
      "PmUG01_12_v1-1398020-1398213",
      "PocGH01_12_v1-1106482-1106671",
      "PvP01_12_v1-1185007-1185184",
      "PKNH_12_v2-0198893-0199083",
      # M1.addon mitochondrial targets
      "PKNH_MIT_v2-0002298-0002488",
      "PmUG01_MIT_v1-0000606-0000784",
      "PocGH01_MIT_v2-0000358-0000550",
      "PvP01_MIT_v2-0004470-0004642"
    )
  )
}

#' @export
print.ampseqQC_panel_settings <- function(x, ...) {
  cat("<ampseqQC panel settings>\n")
  cat("Pool -> reaction:\n")
  reactions <- split(names(x$pool_reactions), x$pool_reactions)
  for (reaction in names(reactions)) {
    cat("  ", reaction, ": ", paste(reactions[[reaction]], collapse = ", "), "\n", sep = "")
  }
  cat("Targets excluded from QC:", length(x$excluded_targets), "\n")
  invisible(x)
}

split_target_pools <- function(panel_information) {
  if (!"pool" %in% names(panel_information)) {
    stop("Panel information has no `pool` column; it should be read from panel_information/amplicon_info.tsv in the Mad4hatter results.", call. = FALSE)
  }
  panel_information %>%
    dplyr::select(target_name, pool) %>%
    tidyr::separate_rows(pool, sep = ",") %>%
    dplyr::mutate(pool = trimws(pool)) %>%
    dplyr::distinct()
}

#' Assign targets to reactions
#'
#' Reads the pools for each target from the `pool` column of the panel
#' information (targets shared by several pools have comma-separated pools) and
#' maps them to reactions using the panel settings. A target shared by pools in
#' different reactions is returned once per reaction, so it counts towards
#' every reaction it was amplified in.
#'
#' @param panel_information Data frame read from
#'   `panel_information/amplicon_info.tsv`.
#' @param settings Panel settings, see [panel_settings()].
#'
#' @return `panel_information` with a `reaction` column, one row per target
#'   and reaction.
#' @export
assign_reactions <- function(panel_information, settings = default_panel_settings()) {
  target_pools <- split_target_pools(panel_information)

  unknown_pools <- setdiff(unique(target_pools$pool), names(settings$pool_reactions))
  if (length(unknown_pools) > 0) {
    stop(
      "No reaction is defined for pool(s): ", paste(unknown_pools, collapse = ", "), ".\n",
      "Supply the reaction for each pool with panel_settings(), e.g. ",
      "panel_settings(c(", paste0('"', unknown_pools, '" = "1"', collapse = ", "), "), base = default_panel_settings())",
      call. = FALSE
    )
  }

  target_reactions <- target_pools %>%
    dplyr::mutate(reaction = unname(settings$pool_reactions[pool])) %>%
    dplyr::distinct(target_name, reaction)

  panel_information %>%
    dplyr::select(-dplyr::any_of("reaction")) %>%
    dplyr::inner_join(target_reactions, by = "target_name")
}

#' Summarise pools in a run
#'
#' @inheritParams assign_reactions
#' @return A data frame with one row per pool giving its reaction and number of
#'   targets.
#' @export
summarise_pools <- function(panel_information, settings = default_panel_settings()) {
  split_target_pools(panel_information) %>%
    dplyr::count(pool, name = "n_targets") %>%
    dplyr::mutate(reaction = unname(settings$pool_reactions[pool]), .after = pool) %>%
    dplyr::arrange(reaction, pool)
}
