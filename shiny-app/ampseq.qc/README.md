# AmpSeq QC Shiny App

A Shiny application for visualizing and analyzing QC data from ampseq sequencing runs.

## Features

The app provides the following analyses:

1. **Primer Dimer Content**
   - Visualizes input reads vs. % attributed to primer dimers
   - Helps assess amplification efficiency

2. **Balancing Across Batches**
   - Swarm plot of reads output by the pipeline by batch
   - Helps identify sequencing imbalances

3. **Control Summary**
   - **Positive Controls**
     - Polyclonality analysis
     - Read summary
   - **Negative Controls**
     - Read distribution
     - Aggregated reads per target

4. **Amplification Success**
   - Sample Reads vs. Number of Successfully Amplified Loci
   - Parasitemia vs. Number of Successfully Amplified Loci

5. **Plate Maps**
   - Plate Layout
   - Reads Heatmap
   - Successful Amplification Heatmap

6. **Summary Tables**
   - Reprep/Repool Summary
   - Polyclonal Information
   - Negative Control Information

## Required Input Files

1. **Results Directory** containing:
   - `sample_coverage.txt`
   - `amplicon_coverage.txt`
   - `allele_data.txt`

2. **Sample Manifest** with the following fields:
   - `SampleID` - Unique identifier for each sample
   - `SampleType` - Specifies whether the entry is a sample, positive control, or negative control
   - `Batch` - Identifies a group of samples processed simultaneously
   - `Column` - The well column where the sample was placed in the plate
   - `Row` - The well row where the sample was placed in the plate
   - `Parasitemia` - The qPCR value for the sample (optional)

## Parameters

The app allows customization of several parameters:

- **Read Threshold**: Minimum reads required for successful amplification
- **Read Filter**: Minimum reads per ASV for positive control analysis
- **Allele Frequency Filter**: Minimum allele frequency per ASV for positive control analysis
- **Negative Control Read Threshold**: Threshold for negative control analysis
- **Reprep/Repool Thresholds**: Thresholds for sample quality assessment
- **Sample ID Standardization**: Option to standardize sample IDs

## Installation

1. Install required R packages:
```R
install.packages(c("shiny", "shinyFiles", "ggplot2", "dplyr", "tidyr", 
                  "ggbeeswarm", "stringr", "plotly", "kableExtra"))
```

2. Clone or download this repository

3. Run the app:
```R
shiny::runApp("path/to/ampseq.qc")
```

## Usage

1. Launch the app
2. Select the results directory containing the required input files
3. Upload the sample manifest file
4. Adjust parameters as needed
5. Navigate through the different tabs to view analyses

## Output

The app generates various visualizations and tables that can be used to:
- Assess the quality of the sequencing run
- Identify potential issues with samples or controls
- Make decisions about sample reprep/repool
- Monitor batch effects and sequencing efficiency

## License

MIT License 