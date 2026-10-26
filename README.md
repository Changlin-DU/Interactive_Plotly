# Economic Growth and Carbon Emissions Decoupling

## Project overview

For this project, I ask a simple question: have countries been able to grow without increasing carbon emissions at the same pace?

I use World Bank data from 2000 to 2024 to track changes in real GDP per capita and CO2 emissions per capita. I also compare the 2000–2002 and 2022–2024 averages to classify 182 countries into four groups: absolute decoupling, relative decoupling, no decoupling, and no sustained economic growth.

## Repository guide

```text
.
├── code/
│   ├── 00_packages.R
│   ├── 01_download_world_bank.R
│   ├── 02_clean_merge_data.R
│   ├── 03_measure_decoupling.R
│   ├── 04_explore_validate.R
│   ├── 05_build_interactive_plot.R
│   ├── 06_export_blog_figures.R
│   └── run_all.R
├── data/
│   ├── raw/
│   └── processed/
├── results/
│   ├── figures/
│   ├── interactive/
│   └── tables/
├── index.qmd
└── README.md
```

The scripts are numbered in the order they should run:

- `00_packages.R` checks the required R packages.
- `01_download_world_bank.R` downloads the World Bank data.
- `02_clean_merge_data.R` cleans the data and creates the country-year panel.
- `03_measure_decoupling.R` calculates long-run changes and assigns the four classifications.
- `04_explore_validate.R` checks the results, sensitivity test, and possible outliers.
- `05_build_interactive_plot.R` creates the two interactive Plotly files.
- `06_export_blog_figures.R` creates the GIF and PNG used in the post.
- `run_all.R` runs the full six-step analysis pipeline and checks the final outputs.

The downloaded files are stored in `data/raw/`. The cleaned country-year panel and country summary are stored in `data/processed/`.

Final figures are in `results/figures/`, interactive HTML files are in `results/interactive/`, and validation tables and pipeline records are in `results/tables/`.

The `_files` folders inside `results/interactive/` are required by the HTML files and should not be moved or deleted.

## Running the project

Open the R project and run this command from the project root:

```r
source("code/run_all.R")
```

This runs the full analysis and recreates the two interactive visualizations.

The figures used in the written post are created separately with:

```r
source("code/06_export_blog_figures.R")
```

To open the interactive visualizations from R:

```r
browseURL("results/interactive/global_decoupling_animation.html")

browseURL("results/interactive/long_run_decoupling_explorer.html")
```

## Main outputs

- `results/interactive/global_decoupling_animation.html`
- `results/interactive/long_run_decoupling_explorer.html`
- `results/figures/global_decoupling_animation.gif`
- `results/figures/long_run_decoupling_focused.png`

## Classification rules

Countries with GDP growth at or below zero are classified as having no sustained economic growth. For countries with positive GDP growth:

- CO2 growth below zero is absolute decoupling.
- CO2 growth below GDP growth is relative decoupling.
- CO2 growth equal to or greater than GDP growth is no decoupling.