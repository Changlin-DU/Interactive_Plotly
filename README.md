# Economic Growth and Carbon Emissions Decoupling

## Summary

For this project, I look at whether countries have increased real GDP per capita without a proportional increase in CO₂ emissions per capita. The analysis covers 182 countries and uses World Bank data from 2000 to 2024.

The final results are presented in two interactive Plotly visualizations. One shows how countries move over time, and the other compares their long-run GDP and CO₂ growth rates.

## Repository structure

```text
blogpost7_Interactive_Plotly/
├── README.md
├── blogpost7_Interactive_Plotly.Rproj
├── code/
│   ├── 00_packages.R
│   ├── 01_download_world_bank.R
│   ├── 02_clean_merge_data.R
│   ├── 03_measure_decoupling.R
│   ├── 04_explore_validate.R
│   ├── 05_build_interactive_plot.R
│   └── run_all.R
├── data/
│   ├── raw/
│   └── processed/
└── results/
    ├── figures/
    ├── interactive/
    └── tables/
```

### Code

The scripts are numbered in the order in which they are used.

| File | What it does |
|---|---|
| `00_packages.R` | Checks the project setup, installs required packages, and tests the World Bank API connection. |
| `01_download_world_bank.R` | Downloads the indicators and country metadata. |
| `02_clean_merge_data.R` | Cleans the raw files and creates the country-year panel. |
| `03_measure_decoupling.R` | Calculates long-run GDP and CO₂ changes and classifies each country. |
| `04_explore_validate.R` | Checks the results, possible outliers, and sensitivity to the comparison window. |
| `05_build_interactive_plot.R` | Creates the two final Plotly visualizations. |
| `run_all.R` | Runs the full project from beginning to end and checks the final outputs. |

### Data

`data/raw/` contains the files downloaded from the World Bank API, along with the download manifest and indicator metadata.

`data/processed/` contains the cleaned country metadata, country-year panel, core analysis panel, and country-level decoupling results.

The two data folders also contain their own README files with more information about the saved files.

### Results

`results/interactive/` contains the two final HTML visualizations:

```text
global_decoupling_animation.html
long_run_decoupling_explorer.html
```

`results/figures/` contains static figures used to check the analysis.

`results/tables/` contains classification summaries, data-quality checks, sensitivity results, excluded observations, package versions, and pipeline records.

## Running the project

Open `blogpost7_Interactive_Plotly.Rproj` in RStudio. Make sure the project folder is the current working directory, then run:

```r
source("code/run_all.R")
```

This runs all six analysis scripts in order. It also checks that the required data, tables, figures, and HTML files were created.

The data download requires an internet connection. The World Bank API does not require an API key.

A successful run ends with:

```text
COMPLETE PIPELINE FINISHED SUCCESSFULLY
```

The pipeline records are saved in:

```text
results/tables/pipeline_run_log.csv
results/tables/pipeline_output_manifest.csv
results/tables/pipeline_session_info.txt
```

Individual scripts can also be run separately, but they should follow their numbered order.

## Interactive visualizations

### Growth and Carbon Paths, 2000–2024

This animation plots real GDP per capita against CO₂ emissions per capita. The slider moves through two-year intervals, colors identify World Bank regions, and bubble size represents population.

```r
browseURL(
  "results/interactive/global_decoupling_animation.html"
)
```

### Which Countries Decoupled Growth from Carbon?

This visualization compares annualized GDP and CO₂ changes between the 2000–2002 and 2022–2024 averages. The focused view shows the main distribution, while the full view includes the two largest emissions-growth outliers.

```r
browseURL(
  "results/interactive/long_run_decoupling_explorer.html"
)
```

## Data and classification

The data come from the [World Bank Indicators API](https://api.worldbank.org/v2/). The analysis uses real GDP per capita, CO₂ emissions per capita, population, renewable electricity share, and World Bank country classifications.

Countries are grouped into four categories:

- **Absolute decoupling:** GDP per capita increased while CO₂ emissions per capita decreased.
- **Relative decoupling:** GDP and CO₂ both increased, but CO₂ increased more slowly.
- **No decoupling:** CO₂ increased at least as quickly as GDP.
- **No sustained economic growth:** GDP per capita did not increase.

Three-year averages are used at both ends of the comparison so that the classification does not depend on a single unusual year.