# Climate Change Literature Perspective: A Bibliometric Analysis

This repository contains the R code and methodology used to analyze metadata from a large-scale systematic literature search related to climate change. The outputs of this processing step have been used to shape a broader academic perspective on climate change research.

## Project Overview

The core objective of this code is to take raw metadata extracted from **Scopus** and refine it by:
1. **Cleaning:** Removing duplicate entries based on unique identifiers (EIDs).
2. **Feature Engineering - Geography:** Extracting the first author's country of affiliation from the raw affiliation strings.
3. **Feature Engineering - Economy:** Mapping the extracted countries to the 2024 **World Bank Income Groups** (High, Upper-middle, Lower-middle, and Low income).
4. **Exporting:** Saving a refined dataset for downstream analysis.
5. **Visualization:** Plotting the absolute number and share of climate change articles published across different income groups from 1946 to 2024.

## Data Source

The data was sourced from **Scopus** (Advanced Document Search) using the following query:

```text
TITLE-ABS-KEY ( "climate change*" OR "global warming" OR "sea level rise" OR "sea-level rise" OR "rising sea level*" ) 
AND ( LIMIT-TO ( SRCTYPE , "j" ) ) 
AND ( LIMIT-TO ( PUBSTAGE , "final" ) ) 
AND ( EXCLUDE ( DOCTYPE , "rp" ) OR EXCLUDE ( DOCTYPE , "tb" ) OR EXCLUDE ( DOCTYPE , "dp" ) OR EXCLUDE ( DOCTYPE , "cr" ) OR EXCLUDE ( DOCTYPE , "er" ) OR EXCLUDE ( DOCTYPE , "bk" ) OR EXCLUDE ( DOCTYPE , "ch" ) OR EXCLUDE ( DOCTYPE , "cp" ) )
```

> **Note on Data Hosting:**
> The raw bibliographic data and the refined datasets are extremely large (multiple Gigabytes). They are currently being tracked and pushed to this repository using **Git LFS (Large File Storage)**. 

## Structure

* `code.R`: The highly structured and professional main R script. It handles data ingestion, rule-based data cleaning, income level classification, data exports, and plotting.
* `ClimateChangeMetadata.zip`: The raw input data containing ~1.79 million records. *(Tracked via Git LFS)*
* `ClimateChangeMetadata_Refined.csv`: The clean data containing ~543,000 unique, classified records. *(Tracked via Git LFS)*

## Requirements to Run the Code

To execute the scripts in this repository, you need:
- R (version 4.0 or higher recommended)
- `dplyr`
- `data.table`
- `stringr`
- `ggplot2`
- `RColorBrewer`
- `scales`

You can install these dependencies in your R console via:
```r
install.packages(c("dplyr", "data.table", "stringr", "ggplot2", "RColorBrewer", "scales"))
```

## How to Run

1. Clone this repository (Make sure `git-lfs` is installed to fetch the CSV files).
2. Set your Working Directory in R to the cloned repository.
3. Run or source the `code.R` script:
   ```r
   source("code.R")
   ```
4. The script will automatically parse `ClimateChangeMetadata.csv`, print output logs to the console, save `ClimateChangeMetadata_Refined.csv`, and prepare two `ggplot` objects (`plot1` and `plot2`) that you can print to your graphics device.
