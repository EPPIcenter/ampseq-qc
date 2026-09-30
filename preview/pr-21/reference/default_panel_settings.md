# Default panel settings for MAD4HatTeR and PfPHAST

Covers the current, versioned and legacy pool names supported by
Mad4hatter for the MAD4HatTeR (D1, R1, R2) and PfPHAST (M1, M1.addon,
M2) panels, using the recommended two-reaction layout: reaction 1
contains D1 and R1 (or M1 and M1.addon), reaction 2 contains R2 (or M2).

## Usage

``` r
default_panel_settings()
```

## Value

An object of class `ampseqQC_panel_settings`.

## Details

Targets excluded from QC are the ldh targets for *P. falciparum* and
non-falciparum species (D1.1, R1.1, R1.2, M1.1, including their legacy
names) and the non-falciparum mitochondrial targets in M1.addon.

## Examples

``` r
default_panel_settings()
#> <ampseqQC panel settings>
#> Pool -> reaction:
#>   1: D1, D1.1, 1A, R1, R1.1, R1.2, 1B, 5, M1, M1.1, M1.addon
#>   2: R2, R2.1, 2, M2, M2.1
#> Targets excluded from QC: 14 
```
