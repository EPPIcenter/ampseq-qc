# QC Summary

This code is designed for visualizing and summarizing QC data from targeted amplicon sequencing of Plasmodium. 

## Running the Interactive Document 

The QC report is available in both **Quarto Markdown** (.qmd) and **Jupyter Notebook** (.ipynb) formats. You can choose whichever format is more convenient for your workflow.

**Using the Quarto Markdown File (QC_report.qmd)**
1. Open the .qmd file in an editor like RStudio or Visual Studio Code.
2. Edit the paths to the required input files and any thresholds at the beginning of the document to match your data location.
3. To render the document and generate an HTML report:
    * In RStudio, click Render.
    * In Visual Studio Code, use the Quarto extension to render the document.
4. The rendered HTML report will summarize your QC data and display the visualizations.

**Using the Jupyter Notebook (QC_report.ipynb)**
1. Open the .ipynb file in Jupyter Notebook.
2. Edit the paths to the required input files and any thresholds at the beginning of the document to match your data location.
3. Run through the code interactively.
4. Select **File > Save and Export Notebook As > HTML** to save the rendered notebook as an HTML file.

## Required Inputs 

To proceed, you must provide the **results directory** from the Mad4hatter pipeline, which should include the following files:

* **sample_coverage.txt**
* **amplicon_coverage.txt**
* **allele_data.txt**

Additionally, a **sample manifest** (CSV) is required. This file must contain the following fields:

* **SampleID** – Unique identifier for each sample.
* **SampleType** – Specifies whether the entry is a **sample**, **positive** control, or **negative** control.
* **Batch** – Identifies a group of samples processed simultaneously by the same individual.
* **Column** – The well column where the sample was placed in the plate.
* **Row** – The well row where the sample was placed in the plate.
* **Parasitemia** – The qPCR value for the sample.

## Development 
If you'd like to make changes to the Jupyter Notebook, follow these steps:
1. Edit the Notebook (QC_report.ipynb) as needed.
2. Once finished, convert the notebook to a Quarto Markdown file:
```bash
quarto convert QC_report.ipynb
```
3. In the converted .qmd file, ensure the following YAML header is present at the top for proper rendering:
```yaml
---
title: "QC Report"
author: ""
output: 
  html_document:
    code-fold: true
    strip-comments: true
    toc: true
execute: 
  echo: false
---
```
This will ensure that the report is formatted correctly when rendered as HTML.

## Installation

To run the Shiny app, follow these steps:

1. Clone the repository to your local machine:

```bash
git clone <repository_url>
```
2. Install dependencies by running the following command in R:

```R
install.packages(c("shiny", "ggplot2", "dplyr", "quarto"))
```
3. Run the app with the following command:

```bash
Rscript app.R
```
This will launch the Shiny app on your local machine where you can interactively explore and visualize your QC data.

## Future Development 
* Extract out the functions as a package 
* Turn this into an R shiny app 
* Make sure that it works with multiple plates etc and plots format correctly
* Add option for well instead of row, column
* manifest could be tsv, csv...
* Write a generate manifest function which populates sampleID with the columns from madhatter repo
