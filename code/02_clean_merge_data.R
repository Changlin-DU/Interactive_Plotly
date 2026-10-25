# ============================================================
# Clean and merge the World Bank indicators
# Project: Economic Growth and Carbon Emissions Decoupling
# ============================================================

# This script:
# 1. Reads all raw World Bank files
# 2. Identifies metadata columns robustly
# 3. Removes World Bank regional and income aggregates
# 4. Merges four indicators into a country-year panel
# 5. Measures annual data coverage
# 6. Selects a stable analytical period
# 7. Saves cleaned and analysis-ready files


# 1. Confirm the project directory ----------------------------------------

expected_project <- "blogpost7_Interactive_Plotly"
current_project <- basename(normalizePath(getwd()))

if (current_project != expected_project) {
  stop(
    "Wrong working directory.\n",
    "Open blogpost7_Interactive_Plotly.Rproj and run this script again.\n",
    "Current directory: ",
    normalizePath(getwd())
  )
}


# 2. Load required packages -----------------------------------------------

packages_needed <- c(
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "janitor"
)

missing_packages <- packages_needed[
  !vapply(
    packages_needed,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Run code/00_packages.R first. Missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(janitor)
})


# 3. Define input files ----------------------------------------------------

raw_files <- c(
  country_metadata =
    "data/raw/country_metadata.csv",
  gdp_per_capita_ppp =
    "data/raw/gdp_per_capita_ppp.csv",
  co2_per_capita =
    "data/raw/co2_per_capita.csv",
  population =
    "data/raw/population.csv",
  renewable_electricity_share =
    "data/raw/renewable_electricity_share.csv"
)

missing_raw_files <- raw_files[
  !file.exists(raw_files)
]

if (length(missing_raw_files) > 0) {
  stop(
    "Run code/01_download_world_bank.R first. Missing files: ",
    paste(missing_raw_files, collapse = ", ")
  )
}


# 4. Define analysis settings ---------------------------------------------

preferred_analysis_start_year <- 2000L

# The final year must contain at least 80% as many paired GDP-CO2
# observations as the best-covered year.
coverage_threshold_fraction <- 0.80


# 5. Define a robust column-name resolver ---------------------------------

resolve_column <- function(
    data,
    candidates,
    column_description,
    required = TRUE
) {
  
  existing_names <- names(data)
  
  compact_existing_names <- existing_names |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(
      "[^a-z0-9]",
      ""
    )
  
  compact_candidates <- candidates |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(
      "[^a-z0-9]",
      ""
    )
  
  matched_positions <- match(
    compact_candidates,
    compact_existing_names,
    nomatch = 0L
  )
  
  matched_positions <- matched_positions[
    matched_positions > 0L
  ]
  
  if (length(matched_positions) == 0L) {
    
    if (required) {
      stop(
        "Could not identify the ",
        column_description,
        " column.\n",
        "Accepted forms: ",
        paste(candidates, collapse = ", "),
        "\n",
        "Available columns: ",
        paste(existing_names, collapse = ", ")
      )
    }
    
    return(NULL)
  }
  
  existing_names[matched_positions[[1]]]
}


# 6. Define an indicator-reading function ---------------------------------

read_indicator_file <- function(
    file_path,
    new_value_name
) {
  
  raw_data <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    na = c("", "NA")
  ) |>
    janitor::clean_names()
  
  required_columns <- c(
    "country_id",
    "countryiso3code",
    "country_value",
    "date",
    "value"
  )
  
  absent_columns <- setdiff(
    required_columns,
    names(raw_data)
  )
  
  if (length(absent_columns) > 0) {
    stop(
      file_path,
      " is missing required columns: ",
      paste(absent_columns, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(raw_data), collapse = ", ")
    )
  }
  
  cleaned_data <- raw_data |>
    dplyr::transmute(
      country_id =
        as.character(country_id),
      country_name_api =
        as.character(country_value),
      iso3_api =
        dplyr::na_if(
          as.character(countryiso3code),
          ""
        ),
      year =
        suppressWarnings(
          as.integer(date)
        ),
      !!new_value_name :=
        suppressWarnings(
          as.numeric(value)
        )
    )
  
  duplicate_count <- cleaned_data |>
    dplyr::count(
      country_id,
      year,
      name = "number_of_rows"
    ) |>
    dplyr::filter(
      number_of_rows > 1L
    ) |>
    nrow()
  
  if (duplicate_count > 0) {
    stop(
      file_path,
      " contains ",
      duplicate_count,
      " genuine duplicate country-year pairs."
    )
  }
  
  cleaned_data
}


# 7. Read the country metadata --------------------------------------------

country_metadata_raw <- readr::read_csv(
  raw_files[["country_metadata"]],
  show_col_types = FALSE,
  na = c("", "NA")
) |>
  janitor::clean_names()

message(
  "Country metadata columns: ",
  paste(
    names(country_metadata_raw),
    collapse = ", "
  )
)


# 8. Identify metadata columns automatically ------------------------------

metadata_iso3_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "id"
  ),
  column_description = "World Bank ISO3 economy ID"
)

metadata_iso2_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "iso2_code",
    "iso2code",
    "iso2"
  ),
  column_description = "ISO2 economy ID"
)

metadata_name_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "name",
    "country_name"
  ),
  column_description = "country name"
)

metadata_region_id_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "region_id",
    "regionid"
  ),
  column_description = "region ID"
)

metadata_region_value_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "region_value",
    "regionvalue",
    "region_name"
  ),
  column_description = "region name"
)

metadata_income_id_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "income_level_id",
    "incomelevel_id",
    "incomelevelid"
  ),
  column_description = "income-group ID"
)

metadata_income_value_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "income_level_value",
    "incomelevel_value",
    "incomelevelvalue",
    "income_level_name"
  ),
  column_description = "income-group name"
)

metadata_capital_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "capital_city",
    "capitalcity"
  ),
  column_description = "capital city",
  required = FALSE
)

metadata_longitude_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "longitude",
    "long"
  ),
  column_description = "longitude",
  required = FALSE
)

metadata_latitude_column <- resolve_column(
  data = country_metadata_raw,
  candidates = c(
    "latitude",
    "lat"
  ),
  column_description = "latitude",
  required = FALSE
)


# 9. Extract metadata vectors ---------------------------------------------

metadata_iso3 <- as.character(
  country_metadata_raw[[metadata_iso3_column]]
)

metadata_iso2 <- as.character(
  country_metadata_raw[[metadata_iso2_column]]
)

metadata_name <- as.character(
  country_metadata_raw[[metadata_name_column]]
)

metadata_region_id <- as.character(
  country_metadata_raw[[metadata_region_id_column]]
)

metadata_region_value <- as.character(
  country_metadata_raw[[metadata_region_value_column]]
)

metadata_income_id <- as.character(
  country_metadata_raw[[metadata_income_id_column]]
)

metadata_income_value <- as.character(
  country_metadata_raw[[metadata_income_value_column]]
)

if (is.null(metadata_capital_column)) {
  
  metadata_capital <- rep(
    NA_character_,
    nrow(country_metadata_raw)
  )
  
} else {
  
  metadata_capital <- as.character(
    country_metadata_raw[[metadata_capital_column]]
  )
}

if (is.null(metadata_longitude_column)) {
  
  metadata_longitude <- rep(
    NA_real_,
    nrow(country_metadata_raw)
  )
  
} else {
  
  metadata_longitude <- suppressWarnings(
    as.numeric(
      country_metadata_raw[[metadata_longitude_column]]
    )
  )
}

if (is.null(metadata_latitude_column)) {
  
  metadata_latitude <- rep(
    NA_real_,
    nrow(country_metadata_raw)
  )
  
} else {
  
  metadata_latitude <- suppressWarnings(
    as.numeric(
      country_metadata_raw[[metadata_latitude_column]]
    )
  )
}


# 10. Clean metadata and remove aggregates --------------------------------

country_metadata_clean <- tibble::tibble(
  country_id = metadata_iso2,
  iso3_code = metadata_iso3,
  country_name = metadata_name,
  region_code = metadata_region_id,
  region = metadata_region_value,
  income_group_code = metadata_income_id,
  income_group = metadata_income_value,
  capital_city = metadata_capital,
  longitude = metadata_longitude,
  latitude = metadata_latitude
) |>
  dplyr::filter(
    !is.na(country_id),
    country_id != "",
    !is.na(region_code),
    region_code != "",
    stringr::str_to_upper(region_code) != "NA",
    !is.na(region),
    stringr::str_to_lower(region) != "aggregates"
  ) |>
  dplyr::distinct(
    country_id,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    country_name
  )

if (nrow(country_metadata_clean) == 0) {
  stop(
    "No countries remained after removing World Bank aggregates."
  )
}

metadata_duplicate_count <- country_metadata_clean |>
  dplyr::count(
    country_id,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1L
  ) |>
  nrow()

if (metadata_duplicate_count > 0) {
  stop(
    "Clean country metadata contain duplicate country_id values."
  )
}

message(
  "Countries/economies retained: ",
  nrow(country_metadata_clean)
)

message(
  "World Bank aggregates removed: ",
  nrow(country_metadata_raw) -
    nrow(country_metadata_clean)
)


# 11. Read the four indicators --------------------------------------------

gdp_data <- read_indicator_file(
  file_path =
    raw_files[["gdp_per_capita_ppp"]],
  new_value_name =
    "gdp_per_capita_ppp"
)

co2_data <- read_indicator_file(
  file_path =
    raw_files[["co2_per_capita"]],
  new_value_name =
    "co2_per_capita"
)

population_data <- read_indicator_file(
  file_path =
    raw_files[["population"]],
  new_value_name =
    "population"
)

renewable_data <- read_indicator_file(
  file_path =
    raw_files[["renewable_electricity_share"]],
  new_value_name =
    "renewable_electricity_share"
)


# 12. Retain only country-year values -------------------------------------

gdp_values <- gdp_data |>
  dplyr::select(
    country_id,
    year,
    gdp_per_capita_ppp
  )

co2_values <- co2_data |>
  dplyr::select(
    country_id,
    year,
    co2_per_capita
  )

population_values <- population_data |>
  dplyr::select(
    country_id,
    year,
    population
  )

renewable_values <- renewable_data |>
  dplyr::select(
    country_id,
    year,
    renewable_electricity_share
  )


# 13. Merge indicators and metadata ---------------------------------------

country_year_panel <- gdp_values |>
  dplyr::full_join(
    co2_values,
    by = c(
      "country_id",
      "year"
    )
  ) |>
  dplyr::full_join(
    population_values,
    by = c(
      "country_id",
      "year"
    )
  ) |>
  dplyr::full_join(
    renewable_values,
    by = c(
      "country_id",
      "year"
    )
  ) |>
  dplyr::inner_join(
    country_metadata_clean,
    by = "country_id"
  ) |>
  dplyr::relocate(
    country_id,
    iso3_code,
    country_name,
    region_code,
    region,
    income_group_code,
    income_group,
    year
  ) |>
  dplyr::arrange(
    country_name,
    year
  )


# 14. Validate the merged panel -------------------------------------------

panel_duplicate_count <- country_year_panel |>
  dplyr::count(
    country_id,
    year,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1L
  ) |>
  nrow()

if (panel_duplicate_count > 0) {
  stop(
    "The merged panel contains duplicate country-year pairs."
  )
}

if (nrow(country_year_panel) == 0) {
  stop(
    "The merged country-year panel is empty."
  )
}


# 15. Calculate annual data coverage --------------------------------------

total_countries <- nrow(
  country_metadata_clean
)

coverage_by_year <- country_year_panel |>
  dplyr::group_by(
    year
  ) |>
  dplyr::summarise(
    total_country_records =
      dplyr::n_distinct(country_id),
    countries_with_gdp =
      sum(!is.na(gdp_per_capita_ppp)),
    countries_with_co2 =
      sum(!is.na(co2_per_capita)),
    countries_with_population =
      sum(!is.na(population)),
    countries_with_renewables =
      sum(!is.na(renewable_electricity_share)),
    countries_with_gdp_and_co2 =
      sum(
        !is.na(gdp_per_capita_ppp) &
          !is.na(co2_per_capita)
      ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    total_metadata_countries =
      total_countries,
    gdp_and_co2_coverage_rate =
      countries_with_gdp_and_co2 /
      total_metadata_countries
  ) |>
  dplyr::arrange(
    year
  )


# 16. Select a stable final analysis year ---------------------------------

maximum_core_country_count <- max(
  coverage_by_year$countries_with_gdp_and_co2,
  na.rm = TRUE
)

minimum_acceptable_core_count <- ceiling(
  maximum_core_country_count *
    coverage_threshold_fraction
)

eligible_core_years <- coverage_by_year |>
  dplyr::filter(
    year >= preferred_analysis_start_year,
    countries_with_gdp_and_co2 >=
      minimum_acceptable_core_count
  )

if (nrow(eligible_core_years) == 0) {
  stop(
    "No year meets the required GDP-CO2 coverage threshold."
  )
}

analysis_start_year <-
  preferred_analysis_start_year

analysis_end_year <- max(
  eligible_core_years$year
)

message(
  "Stable analysis period selected: ",
  analysis_start_year,
  "–",
  analysis_end_year
)

message(
  "Minimum paired GDP-CO2 country count required: ",
  minimum_acceptable_core_count
)


# 17. Create the core analytical panel ------------------------------------

analysis_panel_core <- country_year_panel |>
  dplyr::filter(
    year >= analysis_start_year,
    year <= analysis_end_year,
    !is.na(gdp_per_capita_ppp),
    !is.na(co2_per_capita),
    gdp_per_capita_ppp > 0,
    co2_per_capita >= 0
  ) |>
  dplyr::mutate(
    log_gdp_per_capita =
      log10(gdp_per_capita_ppp),
    log_co2_per_capita =
      dplyr::if_else(
        co2_per_capita > 0,
        log10(co2_per_capita),
        NA_real_
      ),
    population_millions =
      population / 1000000
  ) |>
  dplyr::arrange(
    country_name,
    year
  )

if (nrow(analysis_panel_core) == 0) {
  stop(
    "The core analytical panel is empty."
  )
}


# 18. Create the missingness summary --------------------------------------

indicator_names <- c(
  "gdp_per_capita_ppp",
  "co2_per_capita",
  "population",
  "renewable_electricity_share"
)

missingness_summary <- tibble::tibble(
  variable = indicator_names,
  missing = c(
    sum(
      is.na(country_year_panel$gdp_per_capita_ppp)
    ),
    sum(
      is.na(country_year_panel$co2_per_capita)
    ),
    sum(
      is.na(country_year_panel$population)
    ),
    sum(
      is.na(
        country_year_panel$renewable_electricity_share
      )
    )
  ),
  nonmissing = c(
    sum(
      !is.na(country_year_panel$gdp_per_capita_ppp)
    ),
    sum(
      !is.na(country_year_panel$co2_per_capita)
    ),
    sum(
      !is.na(country_year_panel$population)
    ),
    sum(
      !is.na(
        country_year_panel$renewable_electricity_share
      )
    )
  )
) |>
  dplyr::mutate(
    total_rows =
      missing + nonmissing,
    percent_nonmissing =
      100 * nonmissing / total_rows
  )


# 19. Create the data-quality summary -------------------------------------

data_quality_summary <- tibble::tibble(
  check = c(
    "Duplicate country-year pairs",
    "Nonpositive GDP-per-capita values",
    "Negative CO2-per-capita values",
    "Nonpositive population values",
    "Renewable-share values below 0",
    "Renewable-share values above 100"
  ),
  flagged_rows = c(
    panel_duplicate_count,
    sum(
      !is.na(
        country_year_panel$gdp_per_capita_ppp
      ) &
        country_year_panel$gdp_per_capita_ppp <= 0
    ),
    sum(
      !is.na(
        country_year_panel$co2_per_capita
      ) &
        country_year_panel$co2_per_capita < 0
    ),
    sum(
      !is.na(
        country_year_panel$population
      ) &
        country_year_panel$population <= 0
    ),
    sum(
      !is.na(
        country_year_panel$renewable_electricity_share
      ) &
        country_year_panel$renewable_electricity_share < 0
    ),
    sum(
      !is.na(
        country_year_panel$renewable_electricity_share
      ) &
        country_year_panel$renewable_electricity_share > 100
    )
  )
)


# 20. Record the analysis settings ----------------------------------------

analysis_settings <- tibble::tibble(
  setting = c(
    "preferred_analysis_start_year",
    "selected_analysis_end_year",
    "coverage_threshold_fraction",
    "maximum_paired_country_count",
    "minimum_acceptable_paired_country_count",
    "countries_in_metadata",
    "countries_in_core_analysis",
    "core_analysis_rows"
  ),
  value = c(
    as.character(
      analysis_start_year
    ),
    as.character(
      analysis_end_year
    ),
    as.character(
      coverage_threshold_fraction
    ),
    as.character(
      maximum_core_country_count
    ),
    as.character(
      minimum_acceptable_core_count
    ),
    as.character(
      nrow(country_metadata_clean)
    ),
    as.character(
      dplyr::n_distinct(
        analysis_panel_core$country_id
      )
    ),
    as.character(
      nrow(analysis_panel_core)
    )
  )
)


# 21. Save the processed outputs ------------------------------------------

readr::write_csv(
  country_metadata_clean,
  "data/processed/country_metadata_clean.csv",
  na = ""
)

readr::write_csv(
  country_year_panel,
  "data/processed/country_year_panel.csv",
  na = ""
)

readr::write_csv(
  analysis_panel_core,
  "data/processed/analysis_panel_core.csv",
  na = ""
)

readr::write_csv(
  coverage_by_year,
  "results/tables/data_coverage_by_year.csv",
  na = ""
)

readr::write_csv(
  missingness_summary,
  "results/tables/missingness_summary.csv",
  na = ""
)

readr::write_csv(
  data_quality_summary,
  "results/tables/data_quality_summary.csv",
  na = ""
)

readr::write_csv(
  analysis_settings,
  "results/tables/analysis_settings.csv",
  na = ""
)


# 22. Verify the saved outputs --------------------------------------------

expected_output_files <- c(
  "data/processed/country_metadata_clean.csv",
  "data/processed/country_year_panel.csv",
  "data/processed/analysis_panel_core.csv",
  "results/tables/data_coverage_by_year.csv",
  "results/tables/missingness_summary.csv",
  "results/tables/data_quality_summary.csv",
  "results/tables/analysis_settings.csv"
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0) {
  stop(
    "The following processed files were not created: ",
    paste(missing_output_files, collapse = ", ")
  )
}


# 23. Print verification output -------------------------------------------

latest_coverage_year <- max(
  coverage_by_year$year,
  na.rm = TRUE
)

cat("\n")
cat("============================================================\n")
cat("STEP 3 COMPLETED: DATA CLEANED AND MERGED\n")
cat("============================================================\n")

cat(
  "Countries retained:       ",
  nrow(country_metadata_clean),
  "\n",
  sep = ""
)

cat(
  "Analysis period:          ",
  analysis_start_year,
  "–",
  analysis_end_year,
  "\n",
  sep = ""
)

cat(
  "Core analytical rows:     ",
  nrow(analysis_panel_core),
  "\n",
  sep = ""
)

cat(
  "Countries in core panel:  ",
  dplyr::n_distinct(
    analysis_panel_core$country_id
  ),
  "\n",
  sep = ""
)

cat(
  "Duplicate country-years:  ",
  panel_duplicate_count,
  "\n",
  sep = ""
)

cat(
  "Processed data:           ",
  "data/processed/analysis_panel_core.csv\n",
  sep = ""
)

cat("============================================================\n\n")


cat("Analysis settings:\n")

print(
  analysis_settings,
  n = Inf,
  width = Inf
)


cat("\nIndicator missingness:\n")

print(
  missingness_summary,
  n = Inf,
  width = Inf
)


cat("\nData-quality checks:\n")

print(
  data_quality_summary,
  n = Inf,
  width = Inf
)


cat("\nCoverage in the most recent ten years:\n")

print(
  coverage_by_year |>
    dplyr::filter(
      year >= latest_coverage_year - 9L
    ),
  n = Inf,
  width = Inf
)


cat("\nSample of the cleaned analytical panel:\n")

print(
  analysis_panel_core |>
    dplyr::select(
      country_name,
      iso3_code,
      region,
      income_group,
      year,
      gdp_per_capita_ppp,
      co2_per_capita,
      population,
      renewable_electricity_share
    ) |>
    dplyr::slice_head(
      n = 10
    ),
  n = 10,
  width = Inf
)