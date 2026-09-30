# Add the manifest and reactions to amplicon coverage

Add the manifest and reactions to amplicon coverage

## Usage

``` r
merge_amplicon_coverage(amplicon_coverage, manifest, panel_reactions)
```

## Arguments

- amplicon_coverage:

  Amplicon coverage from Mad4hatter.

- manifest:

  Manifest from
  [`read_manifest()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/read_manifest.md).

- panel_reactions:

  Panel information with reactions, from
  [`assign_reactions()`](https://eppicenter.github.io/ampseq-qc/preview/pr-21/reference/assign_reactions.md).

## Value

Amplicon coverage with manifest columns and a `reaction` column. Targets
in more than one reaction appear once per reaction.
