# QC Summary Shiny App

This Shiny app is designed for visualizing and summarizing QC data from targeted amplicon sequencing of Plasmodium. 

## Installation

To run the app, clone this repository, install requirements and run the following command:

```bash
Rscript app.R
```

## Inputs 

To create the QC plots the results directory from your mad4hatter run and a manifest are required. 
The manifest can include the following columns: 
- SampleID
- SampleType: positive, negative, or sample 
- Batch
- Column
- Row
- Parasitemia (Optional)
