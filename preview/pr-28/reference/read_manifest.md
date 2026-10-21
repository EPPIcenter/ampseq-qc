# Read a sample manifest

The manifest must contain `sample_name`, `SampleType` (`sample`,
`positive` or `negative`), `Batch`, `Column`, `Row` and `Parasitemia`.
Comma and semicolon separated files are supported.

## Usage

``` r
read_manifest(manifest_file)
```

## Arguments

- manifest_file:

  Path to the manifest.

## Value

The manifest as a data frame, with `Row` in upper case.
