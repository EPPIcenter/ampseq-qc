# Add within-sample allele frequency

Add within-sample allele frequency

## Usage

``` r
add_allele_frequency(allele_data, group_cols = c("sample_name", "target_name"))
```

## Arguments

- allele_data:

  An allele table with `reads`.

- group_cols:

  Columns identifying a locus within a sample.

## Value

`allele_data` with an `AlleleFreq` column.
