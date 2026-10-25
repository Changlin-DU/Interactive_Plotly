# ============================================================
# Explore and validate the decoupling results
# Project: Economic Growth and Carbon Emissions Decoupling
# ============================================================

# This script:
# 1. Summarizes classifications by region and income group
# 2. Identifies notable cases and potential outliers
# 3. Creates static diagnostic figures
# 4. Conducts a sensitivity analysis using wider endpoint windows
# 5. Saves all diagnostic results programmatically


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
  "tibble",
  "ggplot2",
  "scales"
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
  library(tibble)
  library(ggplot2)
  library(scales)
})


# 3. Check input files -----------------------------------------------------

required_input_files <- c(
  "data/processed/country_decoupling_summary.csv",
  "data/processed/analysis_panel_decoupling.csv",
  "data/processed/analysis_panel_core.csv"
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0) {
  stop(
    "Run code/03_measure_decoupling.R first. Missing files: ",
    paste(missing_input_files, collapse = ", ")
  )
}


# 4. Read the processed data ----------------------------------------------

decoupling_summary <- readr::read_csv(
  "data/processed/country_decoupling_summary.csv",
  show_col_types = FALSE,
  na = c("", "NA")
)

annual_panel <- readr::read_csv(
  "data/processed/analysis_panel_decoupling.csv",
  show_col_types = FALSE,
  na = c("", "NA")
)

core_panel <- readr::read_csv(
  "data/processed/analysis_panel_core.csv",
  show_col_types = FALSE,
  na = c("", "NA")
)


# 5. Define consistent category order and colors --------------------------

category_levels <- c(
  "Absolute decoupling",
  "Relative decoupling",
  "No decoupling",
  "No sustained economic growth"
)

category_colors <- c(
  "Absolute decoupling" = "#168C68",
  "Relative decoupling" = "#377EB8",
  "No decoupling" = "#D95F02",
  "No sustained economic growth" = "#777777"
)

decoupling_summary <- decoupling_summary |>
  dplyr::mutate(
    decoupling_category = factor(
      decoupling_category,
      levels = category_levels
    )
  )

annual_panel <- annual_panel |>
  dplyr::mutate(
    decoupling_category = factor(
      decoupling_category,
      levels = category_levels
    )
  )


# 6. Validate the main analytical results ---------------------------------

required_summary_columns <- c(
  "country_id",
  "iso3_code",
  "country_name",
  "region",
  "income_group",
  "gdp_total_change_pct",
  "co2_total_change_pct",
  "gdp_cagr_pct",
  "co2_cagr_pct",
  "carbon_intensity_change_pct",
  "decoupling_category"
)

missing_summary_columns <- setdiff(
  required_summary_columns,
  names(decoupling_summary)
)

if (length(missing_summary_columns) > 0) {
  stop(
    "The decoupling summary is missing columns: ",
    paste(missing_summary_columns, collapse = ", ")
  )
}

if (any(is.na(decoupling_summary$decoupling_category))) {
  stop(
    "At least one country has a missing or unrecognized classification."
  )
}

if (
  any(
    !is.finite(decoupling_summary$gdp_cagr_pct)
  ) ||
  any(
    !is.finite(decoupling_summary$co2_cagr_pct)
  )
) {
  stop(
    "At least one country has a nonfinite CAGR."
  )
}

duplicate_country_count <- decoupling_summary |>
  dplyr::count(
    country_id,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1L
  ) |>
  nrow()

if (duplicate_country_count > 0) {
  stop(
    "The decoupling summary contains duplicate countries."
  )
}


# 7. Create classification summary ----------------------------------------

classification_summary <- decoupling_summary |>
  dplyr::count(
    decoupling_category,
    name = "countries",
    .drop = FALSE
  ) |>
  dplyr::mutate(
    percent =
      100 * countries / sum(countries)
  )


# 8. Summarize results by region ------------------------------------------

region_summary <- decoupling_summary |>
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


# 9. Summarize results by income group ------------------------------------

income_summary <- decoupling_summary |>
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


# 10. Calculate category-level statistics ---------------------------------

category_statistics <- decoupling_summary |>
  dplyr::group_by(
    decoupling_category
  ) |>
  dplyr::summarise(
    countries =
      dplyr::n(),
    median_gdp_change_pct =
      median(
        gdp_total_change_pct,
        na.rm = TRUE
      ),
    median_co2_change_pct =
      median(
        co2_total_change_pct,
        na.rm = TRUE
      ),
    median_gdp_cagr_pct =
      median(
        gdp_cagr_pct,
        na.rm = TRUE
      ),
    median_co2_cagr_pct =
      median(
        co2_cagr_pct,
        na.rm = TRUE
      ),
    median_carbon_intensity_change_pct =
      median(
        carbon_intensity_change_pct,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


# 11. Identify notable analytical cases -----------------------------------

largest_gdp_growth <- decoupling_summary |>
  dplyr::slice_max(
    order_by = gdp_cagr_pct,
    n = 10,
    with_ties = FALSE
  ) |>
  dplyr::mutate(
    notable_case =
      "Highest GDP-per-capita growth"
  )

largest_co2_reductions <- decoupling_summary |>
  dplyr::slice_min(
    order_by = co2_cagr_pct,
    n = 10,
    with_ties = FALSE
  ) |>
  dplyr::mutate(
    notable_case =
      "Largest CO2-per-capita decline"
  )

largest_co2_increases <- decoupling_summary |>
  dplyr::slice_max(
    order_by = co2_cagr_pct,
    n = 10,
    with_ties = FALSE
  ) |>
  dplyr::mutate(
    notable_case =
      "Largest CO2-per-capita increase"
  )

notable_cases <- dplyr::bind_rows(
  largest_gdp_growth,
  largest_co2_reductions,
  largest_co2_increases
) |>
  dplyr::select(
    notable_case,
    country_name,
    iso3_code,
    region,
    income_group,
    decoupling_category,
    gdp_total_change_pct,
    co2_total_change_pct,
    gdp_cagr_pct,
    co2_cagr_pct,
    carbon_intensity_change_pct
  ) |>
  dplyr::arrange(
    notable_case,
    country_name
  )


# 12. Identify distributional outliers ------------------------------------

gdp_lower_cutoff <- as.numeric(
  stats::quantile(
    decoupling_summary$gdp_cagr_pct,
    probs = 0.01,
    na.rm = TRUE
  )
)

gdp_upper_cutoff <- as.numeric(
  stats::quantile(
    decoupling_summary$gdp_cagr_pct,
    probs = 0.99,
    na.rm = TRUE
  )
)

co2_lower_cutoff <- as.numeric(
  stats::quantile(
    decoupling_summary$co2_cagr_pct,
    probs = 0.01,
    na.rm = TRUE
  )
)

co2_upper_cutoff <- as.numeric(
  stats::quantile(
    decoupling_summary$co2_cagr_pct,
    probs = 0.99,
    na.rm = TRUE
  )
)

cagr_outliers <- decoupling_summary |>
  dplyr::filter(
    gdp_cagr_pct <= gdp_lower_cutoff |
      gdp_cagr_pct >= gdp_upper_cutoff |
      co2_cagr_pct <= co2_lower_cutoff |
      co2_cagr_pct >= co2_upper_cutoff
  ) |>
  dplyr::mutate(
    outlier_reason =
      dplyr::case_when(
        gdp_cagr_pct <= gdp_lower_cutoff ~
          "Bottom 1% of GDP growth",
        gdp_cagr_pct >= gdp_upper_cutoff ~
          "Top 1% of GDP growth",
        co2_cagr_pct <= co2_lower_cutoff ~
          "Bottom 1% of CO2 growth",
        co2_cagr_pct >= co2_upper_cutoff ~
          "Top 1% of CO2 growth",
        TRUE ~
          "Other"
      )
  ) |>
  dplyr::select(
    country_name,
    iso3_code,
    region,
    income_group,
    decoupling_category,
    gdp_cagr_pct,
    co2_cagr_pct,
    outlier_reason
  ) |>
  dplyr::arrange(
    outlier_reason,
    country_name
  )


# 13. Define a sensitivity-analysis function ------------------------------

classify_with_windows <- function(
    data,
    baseline_years,
    final_years,
    minimum_observations = 3L
) {
  
  baseline_reference_year <- mean(
    baseline_years
  )
  
  final_reference_year <- mean(
    final_years
  )
  
  comparison_years <- final_reference_year -
    baseline_reference_year
  
  baseline <- data |>
    dplyr::filter(
      year %in% baseline_years
    ) |>
    dplyr::group_by(
      country_id,
      country_name
    ) |>
    dplyr::summarise(
      baseline_gdp =
        mean(
          gdp_per_capita_ppp,
          na.rm = TRUE
        ),
      baseline_co2 =
        mean(
          co2_per_capita,
          na.rm = TRUE
        ),
      baseline_n =
        dplyr::n(),
      .groups = "drop"
    )
  
  final <- data |>
    dplyr::filter(
      year %in% final_years
    ) |>
    dplyr::group_by(
      country_id,
      country_name
    ) |>
    dplyr::summarise(
      final_gdp =
        mean(
          gdp_per_capita_ppp,
          na.rm = TRUE
        ),
      final_co2 =
        mean(
          co2_per_capita,
          na.rm = TRUE
        ),
      final_n =
        dplyr::n(),
      .groups = "drop"
    )
  
  baseline |>
    dplyr::inner_join(
      final,
      by = c(
        "country_id",
        "country_name"
      )
    ) |>
    dplyr::filter(
      baseline_n >= minimum_observations,
      final_n >= minimum_observations,
      baseline_gdp > 0,
      final_gdp > 0,
      baseline_co2 > 0,
      final_co2 >= 0
    ) |>
    dplyr::mutate(
      sensitivity_gdp_cagr_pct =
        100 * (
          (
            final_gdp /
              baseline_gdp
          ) ^ (
            1 / comparison_years
          ) -
            1
        ),
      sensitivity_co2_cagr_pct =
        100 * (
          (
            final_co2 /
              baseline_co2
          ) ^ (
            1 / comparison_years
          ) -
            1
        ),
      sensitivity_category =
        dplyr::case_when(
          sensitivity_gdp_cagr_pct <= 0 ~
            "No sustained economic growth",
          sensitivity_gdp_cagr_pct > 0 &
            sensitivity_co2_cagr_pct < 0 ~
            "Absolute decoupling",
          sensitivity_gdp_cagr_pct > 0 &
            sensitivity_co2_cagr_pct >= 0 &
            sensitivity_co2_cagr_pct <
            sensitivity_gdp_cagr_pct ~
            "Relative decoupling",
          sensitivity_gdp_cagr_pct > 0 &
            sensitivity_co2_cagr_pct >=
            sensitivity_gdp_cagr_pct ~
            "No decoupling",
          TRUE ~
            "Unclassified"
        )
    )
}


# 14. Run sensitivity analysis --------------------------------------------

# Primary method:
# 2000-2002 compared with 2022-2024.
#
# Sensitivity method:
# 2000-2004 compared with 2020-2024.
#
# Wider five-year windows reduce the influence of temporary shocks.

sensitivity_results <- classify_with_windows(
  data = core_panel,
  baseline_years = 2000:2004,
  final_years = 2020:2024,
  minimum_observations = 3L
)

sensitivity_comparison <- decoupling_summary |>
  dplyr::select(
    country_id,
    country_name,
    primary_category =
      decoupling_category
  ) |>
  dplyr::inner_join(
    sensitivity_results |>
      dplyr::select(
        country_id,
        sensitivity_gdp_cagr_pct,
        sensitivity_co2_cagr_pct,
        sensitivity_category
      ),
    by = "country_id"
  ) |>
  dplyr::mutate(
    primary_category =
      as.character(primary_category),
    classifications_agree =
      primary_category ==
      sensitivity_category
  ) |>
  dplyr::arrange(
    classifications_agree,
    country_name
  )

sensitivity_agreement_rate <- mean(
  sensitivity_comparison$classifications_agree
)

sensitivity_overview <- tibble::tibble(
  measure = c(
    "Countries compared",
    "Countries with matching classifications",
    "Classification agreement rate"
  ),
  value = c(
    nrow(sensitivity_comparison),
    sum(
      sensitivity_comparison$classifications_agree
    ),
    sensitivity_agreement_rate
  )
)

classification_transitions <- sensitivity_comparison |>
  dplyr::count(
    primary_category,
    sensitivity_category,
    name = "countries"
  ) |>
  dplyr::arrange(
    primary_category,
    sensitivity_category
  )


# 15. Create the CAGR diagnostic scatterplot ------------------------------

label_countries <- c(
  "United States",
  "China",
  "India",
  "Germany",
  "United Kingdom",
  "Denmark",
  "Ukraine",
  "Brazil",
  "South Africa",
  "Somalia, Fed. Rep."
)

scatter_labels <- decoupling_summary |>
  dplyr::filter(
    country_name %in% label_countries
  )

x_limits <- as.numeric(
  stats::quantile(
    decoupling_summary$gdp_cagr_pct,
    probs = c(0.01, 0.99),
    na.rm = TRUE
  )
)

y_limits <- as.numeric(
  stats::quantile(
    decoupling_summary$co2_cagr_pct,
    probs = c(0.01, 0.99),
    na.rm = TRUE
  )
)

cagr_scatter_plot <- ggplot2::ggplot(
  decoupling_summary,
  ggplot2::aes(
    x = gdp_cagr_pct,
    y = co2_cagr_pct,
    color = decoupling_category
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "#666666",
    linewidth = 0.4
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = "#666666",
    linewidth = 0.4
  ) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    color = "#555555",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    alpha = 0.8,
    size = 2.5
  ) +
  ggplot2::geom_text(
    data = scatter_labels,
    ggplot2::aes(
      label = country_name
    ),
    size = 3,
    nudge_y = 0.15,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_manual(
    values = category_colors,
    drop = FALSE
  ) +
  ggplot2::coord_cartesian(
    xlim = x_limits,
    ylim = y_limits
  ) +
  ggplot2::labs(
    title =
      "Economic growth and CO2 emissions followed different paths",
    subtitle =
      "Average annual changes between 2000–2002 and 2022–2024",
    x =
      "Real GDP per capita annual growth (%)",
    y =
      "CO2 emissions per capita annual growth (%)",
    color =
      "Long-run classification",
    caption =
      "Dashed line indicates equal GDP and CO2 growth. Source: World Bank."
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot"
  )


# 16. Create the classification count figure ------------------------------

category_count_plot <- ggplot2::ggplot(
  classification_summary,
  ggplot2::aes(
    x = decoupling_category,
    y = countries,
    fill = decoupling_category
  )
) +
  ggplot2::geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(
        countries,
        "\n",
        round(percent, 1),
        "%"
      )
    ),
    vjust = -0.25,
    size = 3.5
  ) +
  ggplot2::scale_fill_manual(
    values = category_colors,
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(
      mult = c(0, 0.12)
    )
  ) +
  ggplot2::labs(
    title =
      "Two-thirds of classified countries show some form of decoupling",
    subtitle =
      "Long-run classification based on three-year endpoint averages",
    x = NULL,
    y = "Number of countries",
    caption = "Source: World Bank."
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 20,
      hjust = 1
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title.position = "plot"
  )


# 17. Create the regional composition figure ------------------------------

region_composition_plot <- ggplot2::ggplot(
  region_summary,
  ggplot2::aes(
    x = region,
    y = countries,
    fill = decoupling_category
  )
) +
  ggplot2::geom_col(
    position = "fill",
    width = 0.75
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(
    values = category_colors,
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  ggplot2::labs(
    title =
      "Decoupling patterns differ substantially across regions",
    x = NULL,
    y = "Share of classified countries",
    fill = "Classification",
    caption = "Source: World Bank."
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot"
  )


# 18. Save the diagnostic figures -----------------------------------------

ggplot2::ggsave(
  filename =
    "results/figures/cagr_diagnostic_scatter.png",
  plot =
    cagr_scatter_plot,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename =
    "results/figures/decoupling_category_counts.png",
  plot =
    category_count_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename =
    "results/figures/decoupling_by_region.png",
  plot =
    region_composition_plot,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)


# 19. Save all diagnostic tables ------------------------------------------

readr::write_csv(
  classification_summary,
  "results/tables/classification_summary.csv",
  na = ""
)

readr::write_csv(
  region_summary,
  "results/tables/exploratory_region_summary.csv",
  na = ""
)

readr::write_csv(
  income_summary,
  "results/tables/exploratory_income_summary.csv",
  na = ""
)

readr::write_csv(
  category_statistics,
  "results/tables/category_statistics.csv",
  na = ""
)

readr::write_csv(
  notable_cases,
  "results/tables/notable_cases.csv",
  na = ""
)

readr::write_csv(
  cagr_outliers,
  "results/tables/cagr_outliers.csv",
  na = ""
)

readr::write_csv(
  sensitivity_comparison,
  "results/tables/sensitivity_comparison.csv",
  na = ""
)

readr::write_csv(
  sensitivity_overview,
  "results/tables/sensitivity_overview.csv",
  na = ""
)

readr::write_csv(
  classification_transitions,
  "results/tables/classification_transitions.csv",
  na = ""
)


# 20. Verify all saved outputs --------------------------------------------

expected_output_files <- c(
  "results/figures/cagr_diagnostic_scatter.png",
  "results/figures/decoupling_category_counts.png",
  "results/figures/decoupling_by_region.png",
  "results/tables/classification_summary.csv",
  "results/tables/exploratory_region_summary.csv",
  "results/tables/exploratory_income_summary.csv",
  "results/tables/category_statistics.csv",
  "results/tables/notable_cases.csv",
  "results/tables/cagr_outliers.csv",
  "results/tables/sensitivity_comparison.csv",
  "results/tables/sensitivity_overview.csv",
  "results/tables/classification_transitions.csv"
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


# 21. Print verification output -------------------------------------------

cat("\n")
cat("============================================================\n")
cat("STEP 5 COMPLETED: RESULTS EXPLORED AND VALIDATED\n")
cat("============================================================\n")

cat(
  "Countries examined:           ",
  nrow(decoupling_summary),
  "\n",
  sep = ""
)

cat(
  "Potential CAGR outliers:      ",
  nrow(cagr_outliers),
  "\n",
  sep = ""
)

cat(
  "Sensitivity countries:        ",
  nrow(sensitivity_comparison),
  "\n",
  sep = ""
)

cat(
  "Sensitivity agreement rate:   ",
  scales::percent(
    sensitivity_agreement_rate,
    accuracy = 0.1
  ),
  "\n",
  sep = ""
)

cat(
  "Diagnostic figures saved:     ",
  "results/figures/\n",
  sep = ""
)

cat("============================================================\n\n")


cat("Classification summary:\n")

print(
  classification_summary,
  n = Inf,
  width = Inf
)


cat("\nCategory-level statistics:\n")

print(
  category_statistics,
  n = Inf,
  width = Inf
)


cat("\nSensitivity overview:\n")

print(
  sensitivity_overview,
  n = Inf,
  width = Inf
)


cat("\nClassification transitions under sensitivity test:\n")

print(
  classification_transitions,
  n = Inf,
  width = Inf
)


cat("\nPotential CAGR outliers:\n")

print(
  cagr_outliers,
  n = Inf,
  width = Inf
)