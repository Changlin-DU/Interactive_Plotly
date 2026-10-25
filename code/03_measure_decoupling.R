# ============================================================
# Define and calculate economic-emissions decoupling
# Project: Economic Growth and Carbon Emissions Decoupling
# ============================================================

# Long-run comparison:
# Baseline window: 2000-2002
# Final window:    2022-2024
#
# Each endpoint uses a three-year average to reduce sensitivity
# to temporary annual shocks.
#
# Classification:
#
# Absolute decoupling:
#   Real GDP per capita increased while CO2 per capita decreased.
#
# Relative decoupling:
#   Both increased, but CO2 per capita grew more slowly than
#   real GDP per capita.
#
# No decoupling:
#   CO2 per capita grew at least as quickly as real GDP per capita.
#
# No sustained economic growth:
#   Real GDP per capita did not increase over the comparison period.


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
  "tibble"
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
})


# 3. Check the required input file ----------------------------------------

input_file <- "data/processed/analysis_panel_core.csv"

if (!file.exists(input_file)) {
  stop(
    "Missing input file: ",
    input_file,
    "\nRun code/02_clean_merge_data.R first."
  )
}


# 4. Read the cleaned analytical panel ------------------------------------

analysis_panel <- readr::read_csv(
  input_file,
  show_col_types = FALSE,
  na = c("", "NA")
)

required_columns <- c(
  "country_id",
  "iso3_code",
  "country_name",
  "region",
  "income_group",
  "year",
  "gdp_per_capita_ppp",
  "co2_per_capita",
  "population",
  "renewable_electricity_share"
)

missing_columns <- setdiff(
  required_columns,
  names(analysis_panel)
)

if (length(missing_columns) > 0) {
  stop(
    "The analytical panel is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# 5. Validate country-year uniqueness -------------------------------------

input_duplicate_count <- analysis_panel |>
  dplyr::count(
    country_id,
    year,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1L
  ) |>
  nrow()

if (input_duplicate_count > 0) {
  stop(
    "The analytical panel contains duplicate country-year rows."
  )
}


# 6. Define the comparison windows ----------------------------------------

analysis_start_year <- min(
  analysis_panel$year,
  na.rm = TRUE
)

analysis_end_year <- max(
  analysis_panel$year,
  na.rm = TRUE
)

baseline_start_year <- analysis_start_year
baseline_end_year <- analysis_start_year + 2L

final_start_year <- analysis_end_year - 2L
final_end_year <- analysis_end_year

baseline_reference_year <- mean(
  c(
    baseline_start_year,
    baseline_end_year
  )
)

final_reference_year <- mean(
  c(
    final_start_year,
    final_end_year
  )
)

comparison_years <- final_reference_year -
  baseline_reference_year

minimum_years_per_window <- 2L

if (comparison_years <= 0) {
  stop(
    "The comparison period must be greater than zero."
  )
}

message(
  "Baseline window: ",
  baseline_start_year,
  "–",
  baseline_end_year
)

message(
  "Final window: ",
  final_start_year,
  "–",
  final_end_year
)

message(
  "Years between window midpoints: ",
  comparison_years
)


# 7. Calculate baseline averages ------------------------------------------

baseline_summary <- analysis_panel |>
  dplyr::filter(
    year >= baseline_start_year,
    year <= baseline_end_year
  ) |>
  dplyr::group_by(
    country_id,
    iso3_code,
    country_name,
    region,
    income_group
  ) |>
  dplyr::summarise(
    baseline_gdp_per_capita =
      mean(
        gdp_per_capita_ppp,
        na.rm = TRUE
      ),
    baseline_co2_per_capita =
      mean(
        co2_per_capita,
        na.rm = TRUE
      ),
    baseline_population =
      mean(
        population,
        na.rm = TRUE
      ),
    baseline_observations =
      dplyr::n(),
    .groups = "drop"
  )


# 8. Calculate final-period averages --------------------------------------

final_summary <- analysis_panel |>
  dplyr::filter(
    year >= final_start_year,
    year <= final_end_year
  ) |>
  dplyr::group_by(
    country_id,
    iso3_code,
    country_name,
    region,
    income_group
  ) |>
  dplyr::summarise(
    final_gdp_per_capita =
      mean(
        gdp_per_capita_ppp,
        na.rm = TRUE
      ),
    final_co2_per_capita =
      mean(
        co2_per_capita,
        na.rm = TRUE
      ),
    final_population =
      mean(
        population,
        na.rm = TRUE
      ),
    final_observations =
      dplyr::n(),
    .groups = "drop"
  )


# 9. Join the two comparison windows --------------------------------------

country_window_summary <- baseline_summary |>
  dplyr::full_join(
    final_summary,
    by = c(
      "country_id",
      "iso3_code",
      "country_name",
      "region",
      "income_group"
    )
  ) |>
  dplyr::mutate(
    baseline_observations =
      dplyr::coalesce(
        baseline_observations,
        0L
      ),
    final_observations =
      dplyr::coalesce(
        final_observations,
        0L
      )
  )


# 10. Determine eligibility ------------------------------------------------

country_window_summary <- country_window_summary |>
  dplyr::mutate(
    eligible_for_classification =
      baseline_observations >=
      minimum_years_per_window &
      final_observations >=
      minimum_years_per_window &
      !is.na(baseline_gdp_per_capita) &
      !is.na(final_gdp_per_capita) &
      !is.na(baseline_co2_per_capita) &
      !is.na(final_co2_per_capita) &
      baseline_gdp_per_capita > 0 &
      final_gdp_per_capita > 0 &
      baseline_co2_per_capita > 0 &
      final_co2_per_capita >= 0,
    exclusion_reason =
      dplyr::case_when(
        baseline_observations <
          minimum_years_per_window ~
          "Fewer than two baseline observations",
        final_observations <
          minimum_years_per_window ~
          "Fewer than two final-period observations",
        is.na(baseline_gdp_per_capita) |
          is.na(final_gdp_per_capita) ~
          "Missing GDP-per-capita endpoint",
        is.na(baseline_co2_per_capita) |
          is.na(final_co2_per_capita) ~
          "Missing CO2-per-capita endpoint",
        baseline_gdp_per_capita <= 0 |
          final_gdp_per_capita <= 0 ~
          "Nonpositive GDP-per-capita endpoint",
        baseline_co2_per_capita <= 0 ~
          "Nonpositive baseline CO2 value",
        final_co2_per_capita < 0 ~
          "Negative final CO2 value",
        TRUE ~
          NA_character_
      )
  )


# 11. Calculate long-run changes and CAGRs --------------------------------

decoupling_summary <- country_window_summary |>
  dplyr::filter(
    eligible_for_classification
  ) |>
  dplyr::mutate(
    gdp_total_change_pct =
      100 * (
        final_gdp_per_capita /
          baseline_gdp_per_capita -
          1
      ),
    co2_total_change_pct =
      100 * (
        final_co2_per_capita /
          baseline_co2_per_capita -
          1
      ),
    gdp_cagr_pct =
      100 * (
        (
          final_gdp_per_capita /
            baseline_gdp_per_capita
        ) ^ (
          1 / comparison_years
        ) -
          1
      ),
    co2_cagr_pct =
      100 * (
        (
          final_co2_per_capita /
            baseline_co2_per_capita
        ) ^ (
          1 / comparison_years
        ) -
          1
      ),
    carbon_intensity_proxy_baseline =
      baseline_co2_per_capita /
      baseline_gdp_per_capita,
    carbon_intensity_proxy_final =
      final_co2_per_capita /
      final_gdp_per_capita,
    carbon_intensity_change_pct =
      100 * (
        carbon_intensity_proxy_final /
          carbon_intensity_proxy_baseline -
          1
      )
  )


# 12. Classify each eligible country --------------------------------------

decoupling_summary <- decoupling_summary |>
  dplyr::mutate(
    decoupling_category =
      dplyr::case_when(
        gdp_cagr_pct <= 0 ~
          "No sustained economic growth",
        gdp_cagr_pct > 0 &
          co2_cagr_pct < 0 ~
          "Absolute decoupling",
        gdp_cagr_pct > 0 &
          co2_cagr_pct >= 0 &
          co2_cagr_pct < gdp_cagr_pct ~
          "Relative decoupling",
        gdp_cagr_pct > 0 &
          co2_cagr_pct >= gdp_cagr_pct ~
          "No decoupling",
        TRUE ~
          "Unclassified"
      ),
    decoupling_category =
      factor(
        decoupling_category,
        levels = c(
          "Absolute decoupling",
          "Relative decoupling",
          "No decoupling",
          "No sustained economic growth",
          "Unclassified"
        )
      ),
    baseline_window =
      paste0(
        baseline_start_year,
        "–",
        baseline_end_year
      ),
    final_window =
      paste0(
        final_start_year,
        "–",
        final_end_year
      ),
    comparison_years =
      comparison_years
  ) |>
  dplyr::arrange(
    decoupling_category,
    country_name
  )


# 13. Create the exclusions table -----------------------------------------

decoupling_exclusions <- country_window_summary |>
  dplyr::filter(
    !eligible_for_classification
  ) |>
  dplyr::select(
    country_id,
    iso3_code,
    country_name,
    region,
    income_group,
    baseline_observations,
    final_observations,
    baseline_gdp_per_capita,
    final_gdp_per_capita,
    baseline_co2_per_capita,
    final_co2_per_capita,
    exclusion_reason
  ) |>
  dplyr::arrange(
    exclusion_reason,
    country_name
  )


# 14. Join classifications to annual observations -------------------------

analysis_panel_decoupling <- analysis_panel |>
  dplyr::inner_join(
    decoupling_summary |>
      dplyr::select(
        country_id,
        baseline_gdp_per_capita,
        baseline_co2_per_capita,
        final_gdp_per_capita,
        final_co2_per_capita,
        gdp_total_change_pct,
        co2_total_change_pct,
        gdp_cagr_pct,
        co2_cagr_pct,
        carbon_intensity_change_pct,
        decoupling_category
      ),
    by = "country_id"
  ) |>
  dplyr::mutate(
    gdp_index =
      100 * gdp_per_capita_ppp /
      baseline_gdp_per_capita,
    co2_index =
      100 * co2_per_capita /
      baseline_co2_per_capita,
    population_millions =
      population / 1000000,
    renewable_data_available =
      !is.na(
        renewable_electricity_share
      )
  ) |>
  dplyr::arrange(
    country_name,
    year
  )


# 15. Validate the classified panel ---------------------------------------

summary_duplicate_count <- decoupling_summary |>
  dplyr::count(
    country_id,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1L
  ) |>
  nrow()

panel_duplicate_count <- analysis_panel_decoupling |>
  dplyr::count(
    country_id,
    year,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1L
  ) |>
  nrow()

if (summary_duplicate_count > 0) {
  stop(
    "The decoupling summary contains duplicate countries."
  )
}

if (panel_duplicate_count > 0) {
  stop(
    "The classified annual panel contains duplicate country-years."
  )
}

if (
  any(
    is.na(
      analysis_panel_decoupling$decoupling_category
    )
  )
) {
  stop(
    "At least one classified panel row has no decoupling category."
  )
}


# 16. Create summary tables ------------------------------------------------

decoupling_counts <- decoupling_summary |>
  dplyr::count(
    decoupling_category,
    name = "countries",
    .drop = FALSE
  ) |>
  dplyr::mutate(
    percent_of_classified_countries =
      100 * countries / sum(countries)
  )

decoupling_by_region <- decoupling_summary |>
  dplyr::count(
    region,
    decoupling_category,
    name = "countries",
    .drop = FALSE
  ) |>
  dplyr::group_by(
    region
  ) |>
  dplyr::mutate(
    region_total =
      sum(countries),
    percent_within_region =
      100 * countries / region_total
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    region,
    decoupling_category
  )

decoupling_by_income <- decoupling_summary |>
  dplyr::count(
    income_group,
    decoupling_category,
    name = "countries",
    .drop = FALSE
  ) |>
  dplyr::group_by(
    income_group
  ) |>
  dplyr::mutate(
    income_group_total =
      sum(countries),
    percent_within_income_group =
      100 * countries / income_group_total
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    income_group,
    decoupling_category
  )

top_absolute_decouplers <- decoupling_summary |>
  dplyr::filter(
    decoupling_category ==
      "Absolute decoupling"
  ) |>
  dplyr::arrange(
    co2_total_change_pct,
    dplyr::desc(
      gdp_total_change_pct
    )
  ) |>
  dplyr::select(
    country_name,
    iso3_code,
    region,
    income_group,
    gdp_total_change_pct,
    co2_total_change_pct,
    gdp_cagr_pct,
    co2_cagr_pct,
    carbon_intensity_change_pct
  ) |>
  dplyr::slice_head(
    n = 15
  )


# 17. Record the methodology ----------------------------------------------

methodology_settings <- tibble::tibble(
  setting = c(
    "baseline_start_year",
    "baseline_end_year",
    "final_start_year",
    "final_end_year",
    "baseline_reference_year",
    "final_reference_year",
    "comparison_years",
    "minimum_observations_per_window",
    "eligible_countries",
    "excluded_countries"
  ),
  value = c(
    as.character(
      baseline_start_year
    ),
    as.character(
      baseline_end_year
    ),
    as.character(
      final_start_year
    ),
    as.character(
      final_end_year
    ),
    as.character(
      baseline_reference_year
    ),
    as.character(
      final_reference_year
    ),
    as.character(
      comparison_years
    ),
    as.character(
      minimum_years_per_window
    ),
    as.character(
      nrow(decoupling_summary)
    ),
    as.character(
      nrow(decoupling_exclusions)
    )
  )
)


# 18. Save all outputs -----------------------------------------------------

readr::write_csv(
  decoupling_summary,
  "data/processed/country_decoupling_summary.csv",
  na = ""
)

readr::write_csv(
  analysis_panel_decoupling,
  "data/processed/analysis_panel_decoupling.csv",
  na = ""
)

readr::write_csv(
  decoupling_exclusions,
  "results/tables/decoupling_exclusions.csv",
  na = ""
)

readr::write_csv(
  decoupling_counts,
  "results/tables/decoupling_counts.csv",
  na = ""
)

readr::write_csv(
  decoupling_by_region,
  "results/tables/decoupling_by_region.csv",
  na = ""
)

readr::write_csv(
  decoupling_by_income,
  "results/tables/decoupling_by_income.csv",
  na = ""
)

readr::write_csv(
  top_absolute_decouplers,
  "results/tables/top_absolute_decouplers.csv",
  na = ""
)

readr::write_csv(
  methodology_settings,
  "results/tables/decoupling_methodology.csv",
  na = ""
)


# 19. Verify the output files ---------------------------------------------

expected_output_files <- c(
  "data/processed/country_decoupling_summary.csv",
  "data/processed/analysis_panel_decoupling.csv",
  "results/tables/decoupling_exclusions.csv",
  "results/tables/decoupling_counts.csv",
  "results/tables/decoupling_by_region.csv",
  "results/tables/decoupling_by_income.csv",
  "results/tables/top_absolute_decouplers.csv",
  "results/tables/decoupling_methodology.csv"
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0) {
  stop(
    "The following output files were not created: ",
    paste(missing_output_files, collapse = ", ")
  )
}


# 20. Print verification output -------------------------------------------

cat("\n")
cat("============================================================\n")
cat("STEP 4 COMPLETED: DECOUPLING MEASURED\n")
cat("============================================================\n")

cat(
  "Baseline window:       ",
  baseline_start_year,
  "–",
  baseline_end_year,
  "\n",
  sep = ""
)

cat(
  "Final window:          ",
  final_start_year,
  "–",
  final_end_year,
  "\n",
  sep = ""
)

cat(
  "Eligible countries:    ",
  nrow(decoupling_summary),
  "\n",
  sep = ""
)

cat(
  "Excluded countries:    ",
  nrow(decoupling_exclusions),
  "\n",
  sep = ""
)

cat(
  "Annual panel rows:     ",
  nrow(analysis_panel_decoupling),
  "\n",
  sep = ""
)

cat(
  "Summary duplicates:   ",
  summary_duplicate_count,
  "\n",
  sep = ""
)

cat(
  "Panel duplicates:     ",
  panel_duplicate_count,
  "\n",
  sep = ""
)

cat(
  "Main output:          ",
  "data/processed/country_decoupling_summary.csv\n",
  sep = ""
)

cat("============================================================\n\n")


cat("Decoupling classifications:\n")

print(
  decoupling_counts,
  n = Inf,
  width = Inf
)


cat("\nMethodology settings:\n")

print(
  methodology_settings,
  n = Inf,
  width = Inf
)


cat("\nTop absolute-decoupling examples:\n")

print(
  top_absolute_decouplers,
  n = 15,
  width = Inf
)


cat("\nExcluded countries, if any:\n")

print(
  decoupling_exclusions,
  n = Inf,
  width = Inf
)