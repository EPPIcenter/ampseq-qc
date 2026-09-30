# Filter an allele table

Keeps alleles with more than `read_filter` reads and, if `filter_wsaf`
is `TRUE`, a within-sample allele frequency above `af_filter`.

## Usage

``` r
filter_allele_table(
  allele_table,
  group_cols = c("sample_name", "target_name"),
  read_filter = 0,
  af_filter = 0.01,
  filter_wsaf = TRUE
)
```

## Arguments

- allele_table:

  Allele or resistance marker table with `reads`.

- group_cols:

  Columns identifying a locus within a sample, used to calculate allele
  frequency.

- read_filter:

  Minimum reads per allele.

- af_filter:

  Minimum within-sample allele frequency.

- filter_wsaf:

  If `FALSE`, only `read_filter` is applied.

## Value

The filtered table, with the same columns as `allele_table`.
