# Panel settings

Panel settings describe how the pools in a Mad4hatter run map onto
multiplex PCR reactions, and which targets should be left out of QC
calculations (they are still kept in the filtered allele outputs).

## Usage

``` r
panel_settings(
  pool_reactions = character(),
  excluded_targets = character(),
  base = NULL
)
```

## Arguments

- pool_reactions:

  Named character vector mapping pool names (as passed to Mad4hatter's
  `--pools`) to reaction labels, e.g. `c(D1 = "1", R1 = "1", R2 = "2")`.

- excluded_targets:

  Character vector of target names to exclude from QC calculations.

- base:

  Optional panel settings to extend. Pools in `pool_reactions` override
  those in `base`, and `excluded_targets` are added to those in `base`.

## Value

An object of class `ampseqQC_panel_settings`.

## Details

The pools present in a run are always taken from the `pool` column of
`panel_information/amplicon_info.tsv`; the settings only need to know
which reaction each pool was amplified in. Pools can be supplied in any
combination or subset.

## Examples

``` r
# Run R1.2 in its own reaction instead of alongside D1
panel_settings(c(R1 = "3", R1.2 = "3"), base = default_panel_settings())
#> <ampseqQC panel settings>
#> Pool -> reaction:
#>   1: D1, D1.1, 1A, R1.1, 1B, 5, M1, M1.1, M1.addon
#>   2: R2, R2.1, 2, M2, M2.1
#>   3: R1, R1.2
#> Targets excluded from QC: 14 

# A bespoke panel
panel_settings(c(poolA = "A", poolB = "B"))
#> <ampseqQC panel settings>
#> Pool -> reaction:
#>   A: poolA
#>   B: poolB
#> Targets excluded from QC: 0 
```
