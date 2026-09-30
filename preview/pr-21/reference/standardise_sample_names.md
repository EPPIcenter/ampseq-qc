# Standardise sample names

Trims the sequencing information Mad4hatter keeps in sample names (a
trailing `_L001` and the final `_`-separated field, e.g. `_S12`) so that
they match the manifest.

## Usage

``` r
standardise_sample_names(sample_names)
```

## Arguments

- sample_names:

  Character vector of sample names.

## Value

Character vector of standardised sample names.

## Examples

``` r
standardise_sample_names(c("sample1_S1_L001", "sample2_S2"))
#> [1] "sample1" "sample2"
```
