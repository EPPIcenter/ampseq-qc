# Remove Illumina FASTQ suffixes from sample names

Sample names in Mad4hatter outputs are the FASTQ file names up to the
read (`_R1`/`_R2`), so they can still carry information added by the
sequencer. This removes only those parts so the names match the
manifest: the sample number (`_S12`) and, if present, the lane that
follows it (`_L001`). A lane without a sample number is not removed, as
Illumina always adds both. Anything else in the name is kept.

## Usage

``` r
standardise_illumina_sample_name(sample_names, keep = character())
```

## Arguments

- sample_names:

  Character vector of sample names.

- keep:

  Names to return unchanged.

## Value

Character vector of standardised sample names.

## Details

A name that ends in `_S` followed by digits but has no sequencer suffix
(for example `B1_S01`) would lose that ending, so names listed in
`keep`, usually the manifest sample names, are returned unchanged.

## Examples

``` r
standardise_illumina_sample_name(c("sample1_S1_L001", "sample_A_S2"))
#> [1] "sample1"  "sample_A"

# Without `keep`, a name ending in _S<number> is trimmed
standardise_illumina_sample_name("B1_S01")
#> [1] "B1"
standardise_illumina_sample_name("B1_S01", keep = "B1_S01")
#> [1] "B1_S01"
```
