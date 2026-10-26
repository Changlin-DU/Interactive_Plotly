# =============================================================================
# 06_export_blog_figures.R
#
# Creates:
#   results/figures/global_decoupling_animation.gif
#   results/figures/long_run_decoupling_focused.png
#
# Run from the project root:
#   source("code/06_export_blog_figures.R")
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "scales",
  "ragg",
  "gifski",
  "here"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them before running this script."
    )
  )
}


# -----------------------------------------------------------------------------
# 2. Paths
# -----------------------------------------------------------------------------

annual_file <- here::here(
  "data",
  "processed",
  "country_year_panel.csv"
)

summary_file <- here::here(
  "data",
  "processed",
  "country_decoupling_summary.csv"
)

figure_directory <- here::here(
  "results",
  "figures"
)

frame_directory <- file.path(
  figure_directory,
  "animation_frames"
)

if (!file.exists(annual_file)) {
  stop("Missing data/processed/country_year_panel.csv.")
}

if (!file.exists(summary_file)) {
  stop("Missing data/processed/country_decoupling_summary.csv.")
}

dir.create(
  figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  frame_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# -----------------------------------------------------------------------------
# 3. Read data
# -----------------------------------------------------------------------------

annual_data <- suppressMessages(
  readr::read_csv(
    annual_file,
    show_col_types = FALSE
  )
)

summary_data <- suppressMessages(
  readr::read_csv(
    summary_file,
    show_col_types = FALSE
  )
)


# -----------------------------------------------------------------------------
# 4. Colors and theme
# -----------------------------------------------------------------------------

region_palette <- c(
  "East Asia & Pacific" = "#B8860B",
  "Europe & Central Asia" = "#004C99",
  "Latin America & Caribbean" = "#006D4C",
  "Middle East, North Africa, Afghanistan & Pakistan" = "#F3A6C8",
  "North America" = "#111111",
  "South Asia" = "#E31A1C",
  "Sub-Saharan Africa" = "#5E2A84"
)

category_palette <- c(
  "Absolute decoupling" = "#006D4C",
  "Relative decoupling" = "#004C99",
  "No decoupling" = "#F3A6C8",
  "No sustained economic growth" = "#D7191C"
)

category_levels <- names(category_palette)

blog_theme <- ggplot2::theme_minimal(
  base_family = "Arial",
  base_size = 14
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 21,
      color = "#111111"
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12.5,
      color = "#333333",
      margin = ggplot2::margin(b = 15)
    ),
    axis.title = ggplot2::element_text(
      face = "bold",
      size = 13,
      color = "#111111"
    ),
    axis.text = ggplot2::element_text(
      size = 11,
      color = "#222222"
    ),
    panel.grid.major = ggplot2::element_line(
      color = "#D8D8D8",
      linewidth = 0.45
    ),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "right",
    legend.title = ggplot2::element_text(
      face = "bold",
      size = 12
    ),
    legend.text = ggplot2::element_text(size = 10.5),
    plot.margin = ggplot2::margin(18, 24, 16, 18)
  )


# -----------------------------------------------------------------------------
# 5. Prepare animation data
# -----------------------------------------------------------------------------

animation_years <- seq(
  from = 2000,
  to = 2024,
  by = 2
)

animation_data <- annual_data |>
  dplyr::filter(
    .data$iso3_code %in% summary_data$iso3_code,
    .data$year %in% animation_years,
    is.finite(.data$gdp_per_capita_ppp),
    is.finite(.data$co2_per_capita),
    is.finite(.data$population),
    .data$gdp_per_capita_ppp > 0,
    .data$co2_per_capita > 0,
    .data$population > 0,
    !is.na(.data$region)
  ) |>
  dplyr::mutate(
    region = factor(
      .data$region,
      levels = names(region_palette)
    )
  )

population_limits <- range(
  animation_data$population,
  na.rm = TRUE
)

frame_files <- character(length(animation_years))


# -----------------------------------------------------------------------------
# 6. Create animation frames
# -----------------------------------------------------------------------------

for (frame_index in seq_along(animation_years)) {
  
  current_year <- animation_years[[frame_index]]
  
  frame_data <- animation_data |>
    dplyr::filter(.data$year == current_year)
  
  frame_plot <- ggplot2::ggplot(
    frame_data,
    ggplot2::aes(
      x = .data$gdp_per_capita_ppp,
      y = .data$co2_per_capita,
      size = .data$population,
      color = .data$region
    )
  ) +
    ggplot2::geom_point(
      alpha = 0.92,
      stroke = 0
    ) +
    ggplot2::scale_x_log10(
      breaks = c(
        500,
        1000,
        3000,
        10000,
        30000,
        100000
      ),
      labels = c(
        "$500",
        "$1k",
        "$3k",
        "$10k",
        "$30k",
        "$100k"
      ),
      expand = ggplot2::expansion(
        mult = c(0.02, 0.04)
      )
    ) +
    ggplot2::scale_y_log10(
      breaks = c(
        0.01,
        0.1,
        1,
        10,
        100
      ),
      labels = c(
        "0.01",
        "0.1",
        "1",
        "10",
        "100"
      ),
      expand = ggplot2::expansion(
        mult = c(0.02, 0.04)
      )
    ) +
    ggplot2::coord_cartesian(
      xlim = c(500, 160000),
      ylim = c(0.01, 150),
      clip = "on"
    ) +
    ggplot2::scale_color_manual(
      values = region_palette,
      drop = FALSE
    ) +
    ggplot2::scale_size_continuous(
      range = c(1.5, 6),
      trans = "sqrt",
      limits = population_limits,
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Growth and Carbon Paths, 2000–2024",
      subtitle = paste0(
        "Year: ",
        current_year,
        "  |  Bubble size represents population."
      ),
      x = paste0(
        "Real GDP per capita ",
        "(constant 2021 international $, log scale)"
      ),
      y = paste0(
        "CO2 emissions per capita ",
        "(metric tons, log scale)"
      ),
      color = "World Bank region"
    ) +
    blog_theme
  
  frame_files[[frame_index]] <- file.path(
    frame_directory,
    sprintf(
      "frame_%04d.png",
      current_year
    )
  )
  
  ggplot2::ggsave(
    filename = frame_files[[frame_index]],
    plot = frame_plot,
    width = 12,
    height = 7.5,
    dpi = 120,
    bg = "white",
    device = ragg::agg_png
  )
}


# -----------------------------------------------------------------------------
# 7. Combine frames into a GIF
# -----------------------------------------------------------------------------

gif_file <- file.path(
  figure_directory,
  "global_decoupling_animation.gif"
)

gifski::gifski(
  png_files = frame_files,
  gif_file = gif_file,
  width = 1440,
  height = 900,
  delay = 0.9,
  loop = TRUE,
  progress = FALSE
)


# -----------------------------------------------------------------------------
# 8. Prepare long-run data
# -----------------------------------------------------------------------------

latest_population <- annual_data |>
  dplyr::filter(
    .data$iso3_code %in% summary_data$iso3_code,
    is.finite(.data$population)
  ) |>
  dplyr::arrange(
    .data$iso3_code,
    .data$year
  ) |>
  dplyr::group_by(.data$iso3_code) |>
  dplyr::summarise(
    plot_population = dplyr::last(.data$population),
    .groups = "drop"
  )

long_run_data <- summary_data |>
  dplyr::left_join(
    latest_population,
    by = "iso3_code"
  ) |>
  dplyr::mutate(
    decoupling_category = factor(
      .data$decoupling_category,
      levels = category_levels
    )
  ) |>
  dplyr::filter(
    is.finite(.data$gdp_cagr_pct),
    is.finite(.data$co2_cagr_pct),
    is.finite(.data$plot_population),
    .data$plot_population > 0
  )

if (nrow(long_run_data) != nrow(summary_data)) {
  stop("At least one classified country is missing from the static figure.")
}


# -----------------------------------------------------------------------------
# 9. Create focused long-run figure
# -----------------------------------------------------------------------------

long_run_plot <- ggplot2::ggplot(
  long_run_data,
  ggplot2::aes(
    x = .data$gdp_cagr_pct,
    y = .data$co2_cagr_pct,
    size = .data$plot_population,
    color = .data$decoupling_category
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "#555555",
    linewidth = 0.55
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = "#555555",
    linewidth = 0.55
  ) +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    color = "#222222",
    linewidth = 0.65,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    alpha = 0.9,
    stroke = 0
  ) +
  ggplot2::coord_cartesian(
    xlim = c(-3, 10),
    ylim = c(-5, 10),
    clip = "on"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(
      -2,
      0,
      2,
      4,
      6,
      8,
      10
    ),
    labels = scales::label_number(
      suffix = "%"
    )
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(
      -4,
      -2,
      0,
      2,
      4,
      6,
      8,
      10
    ),
    labels = scales::label_number(
      suffix = "%"
    )
  ) +
  ggplot2::scale_color_manual(
    values = category_palette,
    drop = FALSE
  ) +
  ggplot2::scale_size_continuous(
    range = c(1.5, 7),
    trans = "sqrt",
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Which Countries Decoupled Growth from Carbon?",
    subtitle = paste(
      "Annualized changes from the 2000–2002 average",
      "to the 2022–2024 average. Bubble size represents population."
    ),
    x = "Real GDP per-capita annual growth (%)",
    y = "CO2 emissions per-capita annual growth (%)",
    color = "Long-run classification"
  ) +
  blog_theme

png_file <- file.path(
  figure_directory,
  "long_run_decoupling_focused.png"
)

ggplot2::ggsave(
  filename = png_file,
  plot = long_run_plot,
  width = 12,
  height = 7.5,
  dpi = 180,
  bg = "white",
  device = ragg::agg_png
)


# -----------------------------------------------------------------------------
# 10. Clean up temporary frames
# -----------------------------------------------------------------------------

unlink(
  frame_directory,
  recursive = TRUE,
  force = TRUE
)


# -----------------------------------------------------------------------------
# 11. Verify outputs
# -----------------------------------------------------------------------------

if (!file.exists(gif_file)) {
  stop("The animated GIF was not created.")
}

if (!file.exists(png_file)) {
  stop("The long-run PNG was not created.")
}

cat("\n")
cat("============================================================\n")
cat("BLOG FIGURES CREATED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Animated GIF: ", gif_file, "\n", sep = "")
cat("Static PNG:   ", png_file, "\n", sep = "")
cat("============================================================\n")