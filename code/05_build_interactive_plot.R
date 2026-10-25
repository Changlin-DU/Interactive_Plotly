# =============================================================================
# 05_build_interactive_plot.R
#
# Creates:
#   1. Animated GDP–CO2 country explorer
#   2. Long-run decoupling classification explorer
#
# Run from the project root:
#   source("code/05_build_interactive_plot.R")
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "plotly",
  "htmlwidgets",
  "scales",
  "tibble",
  "here"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing packages: ",
      paste(missing_packages, collapse = ", "),
      ". Run source(\"code/00_packages.R\") first."
    )
  )
}


# -----------------------------------------------------------------------------
# 2. Paths
# -----------------------------------------------------------------------------

summary_file <- here::here(
  "data",
  "processed",
  "country_decoupling_summary.csv"
)

annual_file <- here::here(
  "data",
  "processed",
  "country_year_panel.csv"
)

interactive_directory <- here::here(
  "results",
  "interactive"
)

table_directory <- here::here(
  "results",
  "tables"
)

dir.create(
  interactive_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(summary_file)) {
  stop("Missing country_decoupling_summary.csv.")
}

if (!file.exists(annual_file)) {
  stop("Missing country_year_panel.csv.")
}


# -----------------------------------------------------------------------------
# 3. Read data
# -----------------------------------------------------------------------------

summary_data_raw <- suppressMessages(
  readr::read_csv(
    summary_file,
    show_col_types = FALSE
  )
)

annual_data_raw <- suppressMessages(
  readr::read_csv(
    annual_file,
    show_col_types = FALSE
  )
)

summary_required_columns <- c(
  "country_name",
  "iso3_code",
  "region",
  "income_group",
  "gdp_cagr_pct",
  "co2_cagr_pct",
  "decoupling_category"
)

annual_required_columns <- c(
  "country_name",
  "iso3_code",
  "region",
  "income_group",
  "year",
  "gdp_per_capita_ppp",
  "co2_per_capita",
  "population",
  "renewable_electricity_share"
)

missing_summary_columns <- setdiff(
  summary_required_columns,
  names(summary_data_raw)
)

missing_annual_columns <- setdiff(
  annual_required_columns,
  names(annual_data_raw)
)

if (length(missing_summary_columns) > 0) {
  stop(
    paste0(
      "Summary data are missing required columns: ",
      paste(missing_summary_columns, collapse = ", ")
    )
  )
}

if (length(missing_annual_columns) > 0) {
  stop(
    paste0(
      "Annual data are missing required columns: ",
      paste(missing_annual_columns, collapse = ", ")
    )
  )
}


# -----------------------------------------------------------------------------
# 4. Validate source data
# -----------------------------------------------------------------------------

summary_duplicate_count <- summary_data_raw |>
  dplyr::count(
    iso3_code,
    name = "rows"
  ) |>
  dplyr::filter(
    rows > 1
  ) |>
  nrow()

annual_duplicate_count <- annual_data_raw |>
  dplyr::count(
    iso3_code,
    year,
    name = "rows"
  ) |>
  dplyr::filter(
    rows > 1
  ) |>
  nrow()

if (summary_duplicate_count > 0) {
  stop("The country summary contains duplicate countries.")
}

if (annual_duplicate_count > 0) {
  stop("The annual panel contains duplicate country-year pairs.")
}

if (
  any(!is.finite(summary_data_raw$gdp_cagr_pct)) ||
  any(!is.finite(summary_data_raw$co2_cagr_pct))
) {
  stop("The country summary contains non-finite growth values.")
}


# -----------------------------------------------------------------------------
# 5. Independently verify classifications
# -----------------------------------------------------------------------------

category_levels <- c(
  "Absolute decoupling",
  "Relative decoupling",
  "No decoupling",
  "No sustained economic growth"
)

classification_check <- summary_data_raw |>
  dplyr::mutate(
    independently_calculated_category = dplyr::case_when(
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
      
      TRUE ~ NA_character_
    )
  )

classification_mismatch_count <- classification_check |>
  dplyr::filter(
    is.na(independently_calculated_category) |
      decoupling_category != independently_calculated_category
  ) |>
  nrow()

if (classification_mismatch_count > 0) {
  stop(
    paste0(
      classification_mismatch_count,
      " classifications do not match the documented rule."
    )
  )
}


# -----------------------------------------------------------------------------
# 6. Join classifications to annual observations
# -----------------------------------------------------------------------------

country_classifications <- summary_data_raw |>
  dplyr::select(
    iso3_code,
    summary_category = decoupling_category
  )

if ("decoupling_category" %in% names(annual_data_raw)) {
  
  annual_data <- annual_data_raw |>
    dplyr::left_join(
      country_classifications,
      by = "iso3_code"
    ) |>
    dplyr::mutate(
      decoupling_category = dplyr::coalesce(
        summary_category,
        as.character(decoupling_category)
      )
    ) |>
    dplyr::select(
      -summary_category
    )
  
} else {
  
  annual_data <- annual_data_raw |>
    dplyr::left_join(
      country_classifications,
      by = "iso3_code"
    ) |>
    dplyr::rename(
      decoupling_category = summary_category
    )
}

missing_joined_categories <- annual_data |>
  dplyr::filter(
    iso3_code %in% summary_data_raw$iso3_code,
    is.na(decoupling_category)
  ) |>
  nrow()

if (missing_joined_categories > 0) {
  stop(
    "Some annual observations could not be matched to a classification."
  )
}


# -----------------------------------------------------------------------------
# 7. Bubble-size helper
# -----------------------------------------------------------------------------

calculate_bubble_diameter <- function(
    population,
    minimum_size,
    maximum_size
) {
  
  transformed_population <- sqrt(population)
  
  valid_values <- transformed_population[
    is.finite(transformed_population)
  ]
  
  if (length(valid_values) == 0) {
    return(
      rep(
        minimum_size,
        length(population)
      )
    )
  }
  
  input_range <- range(
    valid_values,
    na.rm = TRUE
  )
  
  if (diff(input_range) == 0) {
    return(
      rep(
        mean(c(minimum_size, maximum_size)),
        length(population)
      )
    )
  }
  
  scales::rescale(
    transformed_population,
    to = c(minimum_size, maximum_size),
    from = input_range
  )
}


# -----------------------------------------------------------------------------
# 8. Prepare animation data
# -----------------------------------------------------------------------------

animation_years <- seq(
  from = 2000,
  to = 2024,
  by = 2
)

animation_candidates <- annual_data |>
  dplyr::filter(
    iso3_code %in% summary_data_raw$iso3_code,
    year %in% animation_years
  )

animation_excluded_observations <- animation_candidates |>
  dplyr::filter(
    is.na(gdp_per_capita_ppp) |
      !is.finite(gdp_per_capita_ppp) |
      gdp_per_capita_ppp <= 0 |
      is.na(co2_per_capita) |
      !is.finite(co2_per_capita) |
      co2_per_capita <= 0 |
      is.na(population) |
      !is.finite(population) |
      population <= 0 |
      is.na(region) |
      is.na(decoupling_category)
  ) |>
  dplyr::mutate(
    exclusion_reason = dplyr::case_when(
      is.na(gdp_per_capita_ppp) ~
        "Missing GDP per capita",
      
      !is.finite(gdp_per_capita_ppp) ~
        "Non-finite GDP per capita",
      
      gdp_per_capita_ppp <= 0 ~
        "Nonpositive GDP per capita",
      
      is.na(co2_per_capita) ~
        "Missing CO2 per capita",
      
      !is.finite(co2_per_capita) ~
        "Non-finite CO2 per capita",
      
      co2_per_capita <= 0 ~
        "Nonpositive CO2 per capita",
      
      is.na(population) ~
        "Missing population",
      
      !is.finite(population) ~
        "Non-finite population",
      
      population <= 0 ~
        "Nonpositive population",
      
      is.na(region) ~
        "Missing region",
      
      is.na(decoupling_category) ~
        "Missing category",
      
      TRUE ~ "Other"
    )
  ) |>
  dplyr::select(
    country_name,
    iso3_code,
    year,
    gdp_per_capita_ppp,
    co2_per_capita,
    population,
    region,
    decoupling_category,
    exclusion_reason
  )

readr::write_csv(
  animation_excluded_observations,
  file.path(
    table_directory,
    "animation_excluded_observations.csv"
  )
)

animation_data <- animation_candidates |>
  dplyr::filter(
    is.finite(gdp_per_capita_ppp),
    is.finite(co2_per_capita),
    is.finite(population),
    gdp_per_capita_ppp > 0,
    co2_per_capita > 0,
    population > 0,
    !is.na(region),
    !is.na(decoupling_category)
  ) |>
  dplyr::mutate(
    year = as.integer(year),
    
    population_millions =
      population / 1000000,
    
    animation_bubble_diameter =
      calculate_bubble_diameter(
        population,
        minimum_size = 7,
        maximum_size = 25
      ),
    
    renewable_text = dplyr::if_else(
      is.na(renewable_electricity_share),
      "Not available",
      paste0(
        scales::number(
          renewable_electricity_share,
          accuracy = 0.1
        ),
        "%"
      )
    ),
    
    hover_text = paste0(
      "<b>", country_name, "</b>",
      "<br>Year: ", year,
      "<br>Region: ", region,
      "<br>Income group: ", income_group,
      "<br>GDP per capita: $",
      scales::comma(
        gdp_per_capita_ppp,
        accuracy = 1
      ),
      "<br>CO2 per capita: ",
      scales::number(
        co2_per_capita,
        accuracy = 0.01
      ),
      " metric tons",
      "<br>Population: ",
      scales::number(
        population_millions,
        accuracy = 0.1
      ),
      " million",
      "<br>Renewable electricity: ",
      renewable_text,
      "<br><b>Long-run category: ",
      decoupling_category,
      "</b>"
    )
  )


# -----------------------------------------------------------------------------
# 9. Prepare long-run data
# -----------------------------------------------------------------------------

last_nonmissing <- function(x) {
  
  available_values <- x[
    !is.na(x) &
      is.finite(x)
  ]
  
  if (length(available_values) == 0) {
    return(NA_real_)
  }
  
  available_values[length(available_values)]
}

latest_population <- annual_data |>
  dplyr::filter(
    iso3_code %in% summary_data_raw$iso3_code
  ) |>
  dplyr::arrange(
    iso3_code,
    year
  ) |>
  dplyr::group_by(
    iso3_code
  ) |>
  dplyr::summarise(
    latest_population = last_nonmissing(population),
    .groups = "drop"
  )

long_run_data <- summary_data_raw |>
  dplyr::left_join(
    latest_population,
    by = "iso3_code"
  ) |>
  dplyr::filter(
    is.finite(gdp_cagr_pct),
    is.finite(co2_cagr_pct),
    is.finite(latest_population),
    latest_population > 0
  ) |>
  dplyr::mutate(
    decoupling_category = factor(
      decoupling_category,
      levels = category_levels
    ),
    
    population_millions =
      latest_population / 1000000,
    
    long_run_bubble_diameter =
      calculate_bubble_diameter(
        latest_population,
        minimum_size = 7,
        maximum_size = 22
      ),
    
    hover_text = paste0(
      "<b>", country_name, "</b>",
      "<br>Region: ", region,
      "<br>Income group: ", income_group,
      "<br>GDP per-capita annual growth: ",
      scales::number(
        gdp_cagr_pct,
        accuracy = 0.01
      ),
      "%",
      "<br>CO2 per-capita annual growth: ",
      scales::number(
        co2_cagr_pct,
        accuracy = 0.01
      ),
      "%",
      "<br>Latest population: ",
      scales::number(
        population_millions,
        accuracy = 0.1
      ),
      " million",
      "<br><b>Classification: ",
      decoupling_category,
      "</b>"
    )
  )

if (nrow(long_run_data) != nrow(summary_data_raw)) {
  stop(
    "At least one classified country is missing from the explorer."
  )
}

long_run_duplicate_count <- long_run_data |>
  dplyr::count(
    iso3_code,
    name = "rows"
  ) |>
  dplyr::filter(
    rows > 1
  ) |>
  nrow()

if (long_run_duplicate_count > 0) {
  stop(
    "The long-run explorer contains duplicate countries."
  )
}


# -----------------------------------------------------------------------------
# 10. Color palettes
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
  
  # Requested light pink
  "No decoupling" = "#F3A6C8",
  
  "No sustained economic growth" = "#D7191C"
)

unknown_regions <- setdiff(
  unique(animation_data$region),
  names(region_palette)
)

if (length(unknown_regions) > 0) {
  stop(
    paste0(
      "Regions without assigned colors: ",
      paste(unknown_regions, collapse = ", ")
    )
  )
}


# -----------------------------------------------------------------------------
# 11. Long-run axis settings
# -----------------------------------------------------------------------------

focused_x_range <- c(-3, 10)
focused_y_range <- c(-5, 10)

full_x_range <- c(-3, 10)
full_y_range <- c(-6, 36)

focused_x_ticks <- c(
  -2, 0, 2, 4, 6, 8, 10
)

focused_y_ticks <- c(
  -4, -2, 0, 2, 4, 6, 8, 10
)

full_x_ticks <- focused_x_ticks

full_y_ticks <- c(
  -5, 0, 5, 10, 15, 20, 25, 30, 35
)

focused_x_ticktext <- paste0(
  focused_x_ticks,
  "%"
)

focused_y_ticktext <- paste0(
  focused_y_ticks,
  "%"
)

full_x_ticktext <- paste0(
  full_x_ticks,
  "%"
)

full_y_ticktext <- paste0(
  full_y_ticks,
  "%"
)

countries_outside_focused_view <- long_run_data |>
  dplyr::filter(
    gdp_cagr_pct < focused_x_range[1] |
      gdp_cagr_pct > focused_x_range[2] |
      co2_cagr_pct < focused_y_range[1] |
      co2_cagr_pct > focused_y_range[2]
  ) |>
  nrow()


# -----------------------------------------------------------------------------
# 12. Capture Plotly warnings
# -----------------------------------------------------------------------------

plotly_warning_log <- tibble::tibble(
  source = character(),
  message = character()
)

capture_plotly_warnings <- function(
    expression,
    warning_source
) {
  
  withCallingHandlers(
    expression,
    
    warning = function(warning_condition) {
      
      plotly_warning_log <<- dplyr::bind_rows(
        plotly_warning_log,
        tibble::tibble(
          source = warning_source,
          message = conditionMessage(warning_condition)
        )
      )
      
      invokeRestart("muffleWarning")
    }
  )
}


# -----------------------------------------------------------------------------
# 13. First HTML: animated country-year explorer
# -----------------------------------------------------------------------------

animation_plot <- plotly::plot_ly(
  source = "annual_carbon_explorer"
)

for (region_name in names(region_palette)) {
  
  region_data <- animation_data |>
    dplyr::filter(
      region == region_name
    )
  
  if (nrow(region_data) == 0) {
    next
  }
  
  animation_plot <- plotly::add_trace(
    p = animation_plot,
    data = region_data,
    x = ~gdp_per_capita_ppp,
    y = ~co2_per_capita,
    frame = ~year,
    ids = ~iso3_code,
    text = ~hover_text,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    name = region_name,
    legendgroup = region_name,
    showlegend = TRUE,
    cliponaxis = TRUE,
    
    marker = list(
      size = ~animation_bubble_diameter,
      color = unname(
        region_palette[[region_name]]
      ),
      opacity = 0.92,
      sizemode = "diameter",
      
      line = list(
        color = "rgba(0,0,0,0)",
        width = 0
      )
    )
  )
}

animation_plot <- animation_plot |>
  plotly::layout(
    title = list(
      text = paste0(
        "<b>Growth and Carbon Paths, 2000–2024</b>",
        "<br><sup>",
        "Use Play or the year slider. ",
        "Click a region to isolate it; ",
        "hover over a country for detailed information. ",
        "Bubble size represents population.",
        "</sup>"
      ),
      x = 0.02,
      y = 0.98,
      xanchor = "left",
      yanchor = "top"
    ),
    
    xaxis = list(
      title = list(
        text = paste0(
          "Real GDP per capita ",
          "(constant 2021 international $, log scale)"
        ),
        standoff = 12
      ),
      type = "log",
      range = log10(
        c(500, 160000)
      ),
      tickmode = "array",
      tickvals = c(
        500,
        1000,
        3000,
        10000,
        30000,
        100000
      ),
      ticktext = c(
        "$500",
        "$1k",
        "$3k",
        "$10k",
        "$30k",
        "$100k"
      ),
      ticks = "outside",
      ticklen = 5,
      showticklabels = TRUE,
      gridcolor = "#D5D5D5",
      gridwidth = 1,
      zeroline = FALSE,
      automargin = TRUE
    ),
    
    yaxis = list(
      title = list(
        text = paste0(
          "CO2 emissions per capita ",
          "(metric tons, log scale)"
        ),
        standoff = 12
      ),
      type = "log",
      range = log10(
        c(0.01, 150)
      ),
      tickmode = "array",
      tickvals = c(
        0.01,
        0.1,
        1,
        10,
        100
      ),
      ticktext = c(
        "0.01",
        "0.1",
        "1",
        "10",
        "100"
      ),
      ticks = "outside",
      ticklen = 5,
      showticklabels = TRUE,
      gridcolor = "#D5D5D5",
      gridwidth = 1,
      zeroline = FALSE,
      automargin = TRUE
    ),
    
    legend = list(
      title = list(
        text = "<b>World Bank region</b>"
      ),
      orientation = "v",
      x = 1.02,
      xanchor = "left",
      y = 0.98,
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.97)",
      bordercolor = "#999999",
      borderwidth = 1,
      itemsizing = "constant",
      font = list(
        size = 12,
        color = "#111111"
      ),
      itemclick = "toggle",
      itemdoubleclick = "toggleothers"
    ),
    
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "#222222",
      font = list(
        color = "#111111",
        size = 12
      ),
      align = "left"
    ),
    
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    
    font = list(
      family = "Arial, sans-serif",
      color = "#111111",
      size = 12
    ),
    
    margin = list(
      l = 110,
      r = 390,
      t = 110,
      b = 150
    ),
    
    hovermode = "closest"
  ) |>
  
  plotly::animation_opts(
    frame = 850,
    transition = 250,
    redraw = FALSE
  ) |>
  
  plotly::animation_slider(
    currentvalue = list(
      prefix = "Year: ",
      font = list(
        size = 15,
        color = "#222222"
      )
    ),
    
    transition = list(
      duration = 250,
      easing = "cubic-in-out"
    ),
    
    pad = list(
      t = 35,
      b = 5
    )
  ) |>
  
  plotly::animation_button(
    x = 0,
    xanchor = "left",
    yanchor = "top"
  ) |>
  
  plotly::config(
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE,
    
    modeBarButtonsToRemove = c(
      "select2d",
      "lasso2d",
      "toggleSpikelines"
    )
  )


# -----------------------------------------------------------------------------
# 14. Second HTML: long-run classification explorer
# -----------------------------------------------------------------------------

long_run_plot <- plotly::plot_ly(
  source = "long_run_decoupling_explorer"
)

for (category_name in category_levels) {
  
  category_data <- long_run_data |>
    dplyr::filter(
      as.character(decoupling_category) ==
        category_name
    )
  
  if (nrow(category_data) == 0) {
    next
  }
  
  long_run_plot <- plotly::add_trace(
    p = long_run_plot,
    data = category_data,
    x = ~gdp_cagr_pct,
    y = ~co2_cagr_pct,
    text = ~hover_text,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    name = category_name,
    legendgroup = category_name,
    showlegend = TRUE,
    cliponaxis = TRUE,
    
    marker = list(
      size = ~long_run_bubble_diameter,
      color = unname(
        category_palette[[category_name]]
      ),
      opacity = 0.90,
      sizemode = "diameter",
      
      line = list(
        color = "rgba(0,0,0,0)",
        width = 0
      )
    )
  )
}


# x = 0, y = 0, and y = x reference lines
reference_shapes <- list(
  list(
    type = "line",
    xref = "x",
    yref = "y",
    x0 = 0,
    x1 = 0,
    y0 = full_y_range[1],
    y1 = full_y_range[2],
    line = list(
      color = "#555555",
      width = 1.5
    ),
    layer = "below"
  ),
  
  list(
    type = "line",
    xref = "x",
    yref = "y",
    x0 = full_x_range[1],
    x1 = full_x_range[2],
    y0 = 0,
    y1 = 0,
    line = list(
      color = "#555555",
      width = 1.5
    ),
    layer = "below"
  ),
  
  list(
    type = "line",
    xref = "x",
    yref = "y",
    x0 = full_x_range[1],
    x1 = full_x_range[2],
    y0 = full_x_range[1],
    y1 = full_x_range[2],
    line = list(
      color = "#222222",
      width = 1.5,
      dash = "dash"
    ),
    layer = "below"
  )
)

long_run_plot <- long_run_plot |>
  plotly::layout(
    title = list(
      text = paste0(
        "<b>Which Countries Decoupled Growth from Carbon?</b>",
        "<br><sup>",
        "Annualized changes from the 2000–2002 average ",
        "to the 2022–2024 average. ",
        "Bubble size represents population.",
        "</sup>"
      ),
      x = 0.02,
      y = 0.98,
      xanchor = "left",
      yanchor = "top"
    ),
    
    xaxis = list(
      title = list(
        text = "Real GDP per-capita annual growth (%)",
        standoff = 15
      ),
      
      range = focused_x_range,
      autorange = FALSE,
      
      tickmode = "array",
      tickvals = focused_x_ticks,
      ticktext = focused_x_ticktext,
      
      ticks = "outside",
      ticklen = 6,
      tickwidth = 1,
      tickcolor = "#555555",
      
      showticklabels = TRUE,
      showline = TRUE,
      mirror = FALSE,
      
      linecolor = "#555555",
      linewidth = 1,
      
      showgrid = TRUE,
      gridcolor = "#D8D8D8",
      gridwidth = 1,
      
      zeroline = FALSE,
      fixedrange = FALSE,
      automargin = TRUE
    ),
    
    yaxis = list(
      title = list(
        text = paste0(
          "CO2 emissions per-capita annual growth (%)"
        ),
        standoff = 15
      ),
      
      range = focused_y_range,
      autorange = FALSE,
      
      tickmode = "array",
      tickvals = focused_y_ticks,
      ticktext = focused_y_ticktext,
      
      ticks = "outside",
      ticklen = 6,
      tickwidth = 1,
      tickcolor = "#555555",
      
      showticklabels = TRUE,
      showline = TRUE,
      mirror = FALSE,
      
      linecolor = "#555555",
      linewidth = 1,
      
      showgrid = TRUE,
      gridcolor = "#D8D8D8",
      gridwidth = 1,
      
      zeroline = FALSE,
      fixedrange = FALSE,
      automargin = TRUE
    ),
    
    shapes = reference_shapes,
    
    annotations = list(
      list(
        x = 1.02,
        y = 0.52,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        align = "left",
        
        font = list(
          size = 12,
          color = "#111111"
        ),
        
        text = "<b>Choose a view</b>"
      ),
      
      list(
        x = 1.02,
        y = 0.25,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        align = "left",
        
        font = list(
          size = 11,
          color = "#333333"
        ),
        
        text = paste0(
          "<b>How to read</b>",
          "<br>x ≤ 0:",
          "<br>no sustained economic growth",
          "<br><br>x > 0 and y < 0:",
          "<br>absolute decoupling",
          "<br><br>x > 0 and 0 ≤ y < x:",
          "<br>relative decoupling",
          "<br><br>x > 0 and y ≥ x:",
          "<br>no decoupling",
          "<br><br>Dashed line: y = x"
        )
      )
    ),
    
    updatemenus = list(
      list(
        type = "buttons",
        direction = "down",
        
        x = 1.02,
        xanchor = "left",
        y = 0.50,
        yanchor = "top",
        
        showactive = TRUE,
        active = 0,
        
        bgcolor = "#FFFFFF",
        bordercolor = "#555555",
        borderwidth = 1,
        
        font = list(
          size = 12,
          color = "#111111"
        ),
        
        buttons = list(
          list(
            label = "Focused view",
            method = "relayout",
            
            args = list(
              list(
                "xaxis.range" =
                  focused_x_range,
                
                "xaxis.autorange" =
                  FALSE,
                
                "xaxis.tickvals" =
                  focused_x_ticks,
                
                "xaxis.ticktext" =
                  focused_x_ticktext,
                
                "yaxis.range" =
                  focused_y_range,
                
                "yaxis.autorange" =
                  FALSE,
                
                "yaxis.tickvals" =
                  focused_y_ticks,
                
                "yaxis.ticktext" =
                  focused_y_ticktext
              )
            )
          ),
          
          list(
            label = "Show all outliers",
            method = "relayout",
            
            args = list(
              list(
                "xaxis.range" =
                  full_x_range,
                
                "xaxis.autorange" =
                  FALSE,
                
                "xaxis.tickvals" =
                  full_x_ticks,
                
                "xaxis.ticktext" =
                  full_x_ticktext,
                
                "yaxis.range" =
                  full_y_range,
                
                "yaxis.autorange" =
                  FALSE,
                
                "yaxis.tickvals" =
                  full_y_ticks,
                
                "yaxis.ticktext" =
                  full_y_ticktext
              )
            )
          )
        )
      )
    ),
    
    legend = list(
      title = list(
        text = "<b>Long-run classification</b>"
      ),
      
      orientation = "v",
      
      x = 1.02,
      xanchor = "left",
      y = 0.96,
      yanchor = "top",
      
      bgcolor = "rgba(255,255,255,0.97)",
      bordercolor = "#999999",
      borderwidth = 1,
      
      itemsizing = "constant",
      
      font = list(
        size = 12,
        color = "#111111"
      ),
      
      itemclick = "toggle",
      itemdoubleclick = "toggleothers"
    ),
    
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "#222222",
      
      font = list(
        color = "#111111",
        size = 12
      ),
      
      align = "left"
    ),
    
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    
    font = list(
      family = "Arial, sans-serif",
      color = "#111111",
      size = 12
    ),
    
    margin = list(
      l = 120,
      r = 420,
      t = 120,
      b = 115
    ),
    
    hovermode = "closest"
  ) |>
  
  plotly::config(
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE,
    
    modeBarButtonsToRemove = c(
      "select2d",
      "lasso2d",
      "toggleSpikelines"
    )
  )


# -----------------------------------------------------------------------------
# 15. Build and validate widgets
# -----------------------------------------------------------------------------

animation_plot <- capture_plotly_warnings(
  plotly::plotly_build(
    animation_plot
  ),
  warning_source = "Build animation"
)

long_run_plot <- capture_plotly_warnings(
  plotly::plotly_build(
    long_run_plot
  ),
  warning_source = "Build long-run explorer"
)

long_run_trace_count <- length(
  long_run_plot$x$data
)

long_run_trace_lengths <- vapply(
  long_run_plot$x$data,
  function(trace) {
    length(trace$x)
  },
  integer(1)
)

if (long_run_trace_count != 4) {
  stop(
    paste0(
      "Expected 4 long-run traces, but found ",
      long_run_trace_count,
      "."
    )
  )
}

if (sum(long_run_trace_lengths) != nrow(long_run_data)) {
  stop(
    "Not all countries were included in the long-run Plotly traces."
  )
}

if (
  is.null(long_run_plot$x$layout$xaxis$range) ||
  is.null(long_run_plot$x$layout$yaxis$range)
) {
  stop(
    "The long-run Plotly axes were not configured correctly."
  )
}

if (length(long_run_plot$x$layout$shapes) != 3) {
  stop(
    "The three reference lines were not created correctly."
  )
}

if (
  length(
    long_run_plot$x$layout$updatemenus[[1]]$buttons
  ) != 2
) {
  stop(
    "The two long-run view buttons were not created correctly."
  )
}


# -----------------------------------------------------------------------------
# 16. Save HTML files
# -----------------------------------------------------------------------------

animation_output_file <- file.path(
  interactive_directory,
  "global_decoupling_animation.html"
)

long_run_output_file <- file.path(
  interactive_directory,
  "long_run_decoupling_explorer.html"
)

if (file.exists(animation_output_file)) {
  unlink(animation_output_file)
}

if (file.exists(long_run_output_file)) {
  unlink(long_run_output_file)
}

capture_plotly_warnings(
  htmlwidgets::saveWidget(
    widget = animation_plot,
    file = animation_output_file,
    selfcontained = TRUE,
    title = "Growth and Carbon Paths, 2000–2024"
  ),
  warning_source = "Save animation"
)

capture_plotly_warnings(
  htmlwidgets::saveWidget(
    widget = long_run_plot,
    file = long_run_output_file,
    selfcontained = TRUE,
    title = "Which Countries Decoupled Growth from Carbon?"
  ),
  warning_source = "Save long-run explorer"
)


# -----------------------------------------------------------------------------
# 17. Save audit results
# -----------------------------------------------------------------------------

plotly_audit <- tibble::tibble(
  measure = c(
    "Summary file rows",
    "Annual source observations",
    "Animation candidate observations",
    "Animation observations displayed",
    "Animation countries",
    "Animation frames",
    "Animation excluded observations",
    "Long-run countries",
    "Long-run Plotly traces",
    "Long-run Plotly observations",
    "Countries outside focused view",
    "Annual duplicate country-years",
    "Long-run duplicate countries",
    "Classification mismatches"
  ),
  
  value = c(
    nrow(summary_data_raw),
    nrow(annual_data_raw),
    nrow(animation_candidates),
    nrow(animation_data),
    dplyr::n_distinct(animation_data$iso3_code),
    dplyr::n_distinct(animation_data$year),
    nrow(animation_excluded_observations),
    nrow(long_run_data),
    long_run_trace_count,
    sum(long_run_trace_lengths),
    countries_outside_focused_view,
    annual_duplicate_count,
    long_run_duplicate_count,
    classification_mismatch_count
  )
)

readr::write_csv(
  plotly_audit,
  file.path(
    table_directory,
    "plotly_input_audit.csv"
  )
)

unique_warning_messages <- plotly_warning_log |>
  dplyr::distinct(
    message
  )

readr::write_csv(
  plotly_warning_log,
  file.path(
    table_directory,
    "plotly_warning_audit.csv"
  )
)


# -----------------------------------------------------------------------------
# 18. Completion report
# -----------------------------------------------------------------------------

animation_size_mb <- file.info(
  animation_output_file
)$size / 1024^2

explorer_size_mb <- file.info(
  long_run_output_file
)$size / 1024^2

cat("\n")
cat("====================================================================\n")
cat("STEP 6 FINAL: BOTH INTERACTIVE VISUALIZATIONS CREATED\n")
cat("====================================================================\n")

cat(sprintf(
  "%-38s %s\n",
  "Summary countries:",
  nrow(summary_data_raw)
))

cat(sprintf(
  "%-38s %s\n",
  "Animation observations:",
  nrow(animation_data)
))

cat(sprintf(
  "%-38s %s\n",
  "Animation countries:",
  dplyr::n_distinct(animation_data$iso3_code)
))

cat(sprintf(
  "%-38s %s\n",
  "Animation frames:",
  dplyr::n_distinct(animation_data$year)
))

cat(sprintf(
  "%-38s %s\n",
  "Legitimate missing observations:",
  nrow(animation_excluded_observations)
))

cat(sprintf(
  "%-38s %s\n",
  "Long-run countries:",
  nrow(long_run_data)
))

cat(sprintf(
  "%-38s %s\n",
  "Long-run Plotly traces:",
  long_run_trace_count
))

cat(sprintf(
  "%-38s %s\n",
  "Long-run plotted observations:",
  sum(long_run_trace_lengths)
))

cat(sprintf(
  "%-38s %s\n",
  "Countries outside focused view:",
  countries_outside_focused_view
))

cat(sprintf(
  "%-38s %s\n",
  "Classification mismatches:",
  classification_mismatch_count
))

cat(sprintf(
  "%-38s %s\n",
  "Annual duplicate pairs:",
  annual_duplicate_count
))

cat(sprintf(
  "%-38s %s\n",
  "Long-run duplicate countries:",
  long_run_duplicate_count
))

cat(sprintf(
  "%-38s %s\n",
  "Unique Plotly warning messages:",
  nrow(unique_warning_messages)
))

cat(sprintf(
  "%-38s %.2f MB\n",
  "Animation HTML size:",
  animation_size_mb
))

cat(sprintf(
  "%-38s %.2f MB\n",
  "Explorer HTML size:",
  explorer_size_mb
))

cat("====================================================================\n")

cat("\nPlotly audit summary:\n")
print(plotly_audit)

if (nrow(unique_warning_messages) == 0) {
  
  cat("\nNo Plotly warnings were captured.\n")
  
} else {
  
  cat("\nCaptured Plotly warnings:\n")
  print(plotly_warning_log)
}

cat("\nOpen the animation with:\n")
cat(
  'browseURL("results/interactive/global_decoupling_animation.html")\n'
)

cat("\nOpen the long-run explorer with:\n")
cat(
  'browseURL("results/interactive/long_run_decoupling_explorer.html")\n'
)