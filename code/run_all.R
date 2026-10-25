# =============================================================================
# run_all.R
#
# Run the complete Blog Post 7 analysis from the project root.
#
# Run:
#   source("code/run_all.R")
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Confirm project root
# -----------------------------------------------------------------------------

expected_project <- "blogpost7_Interactive_Plotly"

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

if (basename(project_root) != expected_project) {
  stop(
    "Wrong working directory.\n",
    "Expected: ", expected_project, "\n",
    "Current:  ", project_root, "\n\n",
    "Open blogpost7_Interactive_Plotly.Rproj and run again."
  )
}

options(
  repos = c(
    CRAN = "https://cloud.r-project.org"
  ),
  timeout = 300,
  scipen = 999,
  digits = 4
)


# -----------------------------------------------------------------------------
# 2. Define pipeline
# -----------------------------------------------------------------------------

pipeline <- data.frame(
  step = 1:6,
  
  name = c(
    "Packages and API connection",
    "Download World Bank data",
    "Clean and merge data",
    "Measure decoupling",
    "Explore and validate results",
    "Build interactive Plotly files"
  ),
  
  script = c(
    "code/00_packages.R",
    "code/01_download_world_bank.R",
    "code/02_clean_merge_data.R",
    "code/03_measure_decoupling.R",
    "code/04_explore_validate.R",
    "code/05_build_interactive_plot.R"
  ),
  
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# 3. Define required outputs
# -----------------------------------------------------------------------------

required_outputs <- list(
  
  # Step 1
  c(
    "results/tables/package_versions.csv"
  ),
  
  # Step 2
  c(
    "data/raw/country_metadata.csv",
    "data/raw/gdp_per_capita_ppp.csv",
    "data/raw/co2_per_capita.csv",
    "data/raw/population.csv",
    "data/raw/renewable_electricity_share.csv",
    "data/raw/download_manifest.csv",
    "data/raw/duplicate_check.csv"
  ),
  
  # Step 3
  c(
    "data/processed/country_metadata_clean.csv",
    "data/processed/country_year_panel.csv",
    "data/processed/analysis_panel_core.csv",
    "results/tables/analysis_settings.csv",
    "results/tables/data_quality_summary.csv",
    "results/tables/missingness_summary.csv"
  ),
  
  # Step 4
  c(
    "data/processed/analysis_panel_decoupling.csv",
    "data/processed/country_decoupling_summary.csv",
    "results/tables/decoupling_counts.csv",
    "results/tables/decoupling_methodology.csv",
    "results/tables/decoupling_exclusions.csv"
  ),
  
  # Step 5
  c(
    "results/tables/classification_summary.csv",
    "results/tables/category_statistics.csv",
    "results/tables/sensitivity_comparison.csv",
    "results/tables/sensitivity_overview.csv",
    "results/tables/cagr_outliers.csv",
    "results/figures/cagr_diagnostic_scatter.png",
    "results/figures/decoupling_by_region.png",
    "results/figures/decoupling_category_counts.png"
  ),
  
  # Step 6
  c(
    "results/interactive/global_decoupling_animation.html",
    "results/interactive/long_run_decoupling_explorer.html",
    "results/tables/plotly_input_audit.csv",
    "results/tables/plotly_warning_audit.csv",
    "results/tables/animation_excluded_observations.csv"
  )
)


# -----------------------------------------------------------------------------
# 4. Validate pipeline scripts
# -----------------------------------------------------------------------------

missing_scripts <- pipeline$script[
  !file.exists(pipeline$script)
]

if (length(missing_scripts) > 0) {
  stop(
    "Missing pipeline scripts:\n- ",
    paste(
      missing_scripts,
      collapse = "\n- "
    )
  )
}

dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)


# -----------------------------------------------------------------------------
# 5. Initialize pipeline log
# -----------------------------------------------------------------------------

run_started <- Sys.time()

run_id <- format(
  run_started,
  tz = "UTC",
  format = "%Y%m%dT%H%M%SZ"
)

format_utc <- function(x) {
  
  format(
    x,
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%S UTC"
  )
}

pipeline_log <- data.frame(
  run_id = character(),
  step = integer(),
  name = character(),
  script = character(),
  status = character(),
  started_utc = character(),
  finished_utc = character(),
  elapsed_seconds = numeric(),
  warning_count = integer(),
  message = character(),
  stringsAsFactors = FALSE
)

save_log <- function() {
  
  write.csv(
    pipeline_log,
    "results/tables/pipeline_run_log.csv",
    row.names = FALSE,
    na = ""
  )
}


# -----------------------------------------------------------------------------
# 6. Start pipeline
# -----------------------------------------------------------------------------

cat("\n")
cat("====================================================================\n")
cat("STARTING COMPLETE BLOG POST 7 PIPELINE\n")
cat("====================================================================\n")

cat(
  "Project root: ",
  project_root,
  "\n",
  sep = ""
)

cat(
  "Run ID:       ",
  run_id,
  "\n",
  sep = ""
)

cat(
  "Started:      ",
  format_utc(run_started),
  "\n",
  sep = ""
)

cat("====================================================================\n")


# -----------------------------------------------------------------------------
# 7. Run all six scripts
# -----------------------------------------------------------------------------

for (i in seq_len(nrow(pipeline))) {
  
  step_started <- Sys.time()
  
  warning_messages <- character()
  error_message <- NA_character_
  
  cat("\n")
  cat("====================================================================\n")
  
  cat(
    sprintf(
      "PIPELINE STEP %s OF %s: %s\n",
      i,
      nrow(pipeline),
      pipeline$name[i]
    )
  )
  
  cat(
    sprintf(
      "Script: %s\n",
      pipeline$script[i]
    )
  )
  
  cat("====================================================================\n\n")
  
  step_environment <- new.env(
    parent = globalenv()
  )
  
  step_succeeded <- tryCatch(
    {
      withCallingHandlers(
        source(
          pipeline$script[i],
          local = step_environment,
          echo = FALSE,
          chdir = FALSE,
          encoding = "UTF-8"
        ),
        
        warning = function(w) {
          
          warning_messages <<- c(
            warning_messages,
            conditionMessage(w)
          )
          
          invokeRestart("muffleWarning")
        }
      )
      
      TRUE
    },
    
    error = function(e) {
      
      error_message <<- conditionMessage(e)
      
      FALSE
    }
  )
  
  
  # ---------------------------------------------------------------------------
  # Confirm that expected outputs were created
  # ---------------------------------------------------------------------------
  
  if (step_succeeded) {
    
    step_files <- required_outputs[[i]]
    
    missing_files <- step_files[
      !file.exists(step_files)
    ]
    
    existing_files <- step_files[
      file.exists(step_files)
    ]
    
    empty_files <- existing_files[
      file.info(existing_files)$size <= 0
    ]
    
    if (length(missing_files) > 0) {
      
      step_succeeded <- FALSE
      
      error_message <- paste0(
        "Required outputs were not created: ",
        paste(
          missing_files,
          collapse = ", "
        )
      )
      
    } else if (length(empty_files) > 0) {
      
      step_succeeded <- FALSE
      
      error_message <- paste0(
        "Required outputs are empty: ",
        paste(
          empty_files,
          collapse = ", "
        )
      )
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Record step result
  # ---------------------------------------------------------------------------
  
  step_finished <- Sys.time()
  
  warning_messages <- unique(
    warning_messages
  )
  
  if (!step_succeeded) {
    
    status <- "failed"
    result_message <- error_message
    
  } else if (length(warning_messages) > 0) {
    
    status <- "completed_with_warnings"
    
    result_message <- paste(
      warning_messages,
      collapse = " | "
    )
    
  } else {
    
    status <- "completed"
    
    result_message <-
      "Completed with all required outputs present."
  }
  
  pipeline_log <- rbind(
    pipeline_log,
    
    data.frame(
      run_id = run_id,
      step = i,
      name = pipeline$name[i],
      script = pipeline$script[i],
      status = status,
      
      started_utc = format_utc(
        step_started
      ),
      
      finished_utc = format_utc(
        step_finished
      ),
      
      elapsed_seconds = round(
        as.numeric(
          difftime(
            step_finished,
            step_started,
            units = "secs"
          )
        ),
        2
      ),
      
      warning_count =
        length(warning_messages),
      
      message =
        result_message,
      
      stringsAsFactors = FALSE
    )
  )
  
  save_log()
  
  
  # ---------------------------------------------------------------------------
  # Print captured warnings
  # ---------------------------------------------------------------------------
  
  if (length(warning_messages) > 0) {
    
    cat("\nCaptured warnings for this step:\n")
    
    for (j in seq_along(warning_messages)) {
      
      cat(
        sprintf(
          "%s. %s\n",
          j,
          warning_messages[j]
        )
      )
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Stop immediately if a step failed
  # ---------------------------------------------------------------------------
  
  if (!step_succeeded) {
    
    stop(
      "Pipeline stopped at step ",
      i,
      " (",
      pipeline$name[i],
      ").\n",
      "Reason: ",
      error_message,
      "\n",
      "See results/tables/pipeline_run_log.csv.",
      call. = FALSE
    )
  }
}


# -----------------------------------------------------------------------------
# 8. Define final output files
# -----------------------------------------------------------------------------

summary_file <-
  "data/processed/country_decoupling_summary.csv"

audit_file <-
  "results/tables/plotly_input_audit.csv"

warning_file <-
  "results/tables/plotly_warning_audit.csv"

animation_file <-
  "results/interactive/global_decoupling_animation.html"

explorer_file <-
  "results/interactive/long_run_decoupling_explorer.html"


# -----------------------------------------------------------------------------
# 9. Validate final country summary
# -----------------------------------------------------------------------------

summary_data <- read.csv(
  summary_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (nrow(summary_data) == 0) {
  stop(
    "Final validation failed: country summary is empty."
  )
}

if (anyDuplicated(summary_data$iso3_code) > 0) {
  stop(
    "Final validation failed: duplicate countries remain."
  )
}

expected_categories <- c(
  "Absolute decoupling",
  "Relative decoupling",
  "No decoupling",
  "No sustained economic growth"
)

unexpected_categories <- setdiff(
  unique(
    summary_data$decoupling_category
  ),
  expected_categories
)

if (length(unexpected_categories) > 0) {
  stop(
    "Final validation failed: unexpected categories: ",
    paste(
      unexpected_categories,
      collapse = ", "
    )
  )
}


# -----------------------------------------------------------------------------
# 10. Calculate final classification counts
# -----------------------------------------------------------------------------

classification_counts <- as.data.frame(
  table(
    summary_data$decoupling_category
  ),
  stringsAsFactors = FALSE
)

names(classification_counts) <- c(
  "decoupling_category",
  "countries"
)

classification_counts <- classification_counts[
  classification_counts$countries > 0,
  ,
  drop = FALSE
]

if (
  sum(classification_counts$countries) !=
  nrow(summary_data)
) {
  stop(
    paste0(
      "Final validation failed: classification ",
      "counts are inconsistent."
    )
  )
}


# -----------------------------------------------------------------------------
# 11. Validate Plotly audit
# -----------------------------------------------------------------------------

plotly_audit <- read.csv(
  audit_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

audit_value <- function(measure_name) {
  
  value <- plotly_audit$value[
    plotly_audit$measure == measure_name
  ]
  
  if (length(value) != 1) {
    stop(
      "Missing or duplicated audit measure: ",
      measure_name
    )
  }
  
  as.numeric(value)
}

if (
  audit_value("Long-run countries") !=
  nrow(summary_data)
) {
  stop(
    paste0(
      "Final validation failed: long-run ",
      "country count is inconsistent."
    )
  )
}

if (
  audit_value("Long-run Plotly observations") !=
  nrow(summary_data)
) {
  stop(
    paste0(
      "Final validation failed: not all countries ",
      "entered the explorer."
    )
  )
}

if (
  audit_value("Annual duplicate country-years") != 0
) {
  stop(
    paste0(
      "Final validation failed: duplicate ",
      "country-years were detected."
    )
  )
}

if (
  audit_value("Long-run duplicate countries") != 0
) {
  stop(
    paste0(
      "Final validation failed: duplicate ",
      "long-run countries were detected."
    )
  )
}

if (
  audit_value("Classification mismatches") != 0
) {
  stop(
    paste0(
      "Final validation failed: classification ",
      "mismatches were detected."
    )
  )
}


# -----------------------------------------------------------------------------
# 12. Validate Plotly warning audit
# -----------------------------------------------------------------------------

plotly_warnings <- read.csv(
  warning_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (nrow(plotly_warnings) > 0) {
  stop(
    paste0(
      "Final validation failed: Plotly warnings ",
      "were captured."
    )
  )
}


# -----------------------------------------------------------------------------
# 13. Validate HTML files
# -----------------------------------------------------------------------------

minimum_html_size <- 100000

if (
  file.info(animation_file)$size <
  minimum_html_size
) {
  stop(
    paste0(
      "Final validation failed: animation ",
      "HTML appears incomplete."
    )
  )
}

if (
  file.info(explorer_file)$size <
  minimum_html_size
) {
  stop(
    paste0(
      "Final validation failed: explorer ",
      "HTML appears incomplete."
    )
  )
}


# -----------------------------------------------------------------------------
# 14. Save output manifest
# -----------------------------------------------------------------------------

all_output_files <- unique(
  unlist(
    required_outputs,
    use.names = FALSE
  )
)

all_output_information <- file.info(
  all_output_files
)

output_manifest <- data.frame(
  file = all_output_files,
  
  size_bytes =
    all_output_information$size,
  
  modified_utc = format(
    all_output_information$mtime,
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%S UTC"
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  output_manifest,
  "results/tables/pipeline_output_manifest.csv",
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15. Save session information
# -----------------------------------------------------------------------------

writeLines(
  capture.output(
    sessionInfo()
  ),
  "results/tables/pipeline_session_info.txt"
)


# -----------------------------------------------------------------------------
# 16. Completion report
# -----------------------------------------------------------------------------

run_finished <- Sys.time()

elapsed_seconds <- as.numeric(
  difftime(
    run_finished,
    run_started,
    units = "secs"
  )
)

cat("\n")
cat("====================================================================\n")
cat("COMPLETE PIPELINE FINISHED SUCCESSFULLY\n")
cat("====================================================================\n")

cat(
  sprintf(
    "%-34s %s\n",
    "Run ID:",
    run_id
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Completed steps:",
    nrow(pipeline_log)
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Summary countries:",
    nrow(summary_data)
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Classification mismatches:",
    0
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Annual duplicate pairs:",
    0
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Long-run duplicate countries:",
    0
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Plotly warnings:",
    0
  )
)

cat(
  sprintf(
    "%-34s %.2f seconds\n",
    "Total elapsed time:",
    elapsed_seconds
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Pipeline log:",
    "results/tables/pipeline_run_log.csv"
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Output manifest:",
    "results/tables/pipeline_output_manifest.csv"
  )
)

cat(
  sprintf(
    "%-34s %s\n",
    "Session information:",
    "results/tables/pipeline_session_info.txt"
  )
)

cat("====================================================================\n")

cat("\nFinal classification counts:\n")

print(
  classification_counts,
  row.names = FALSE
)

cat("\nOpen the animation with:\n")

cat(
  'browseURL("results/interactive/global_decoupling_animation.html")\n'
)

cat("\nOpen the long-run explorer with:\n")

cat(
  'browseURL("results/interactive/long_run_decoupling_explorer.html")\n'
)

cat(
  paste0(
    "\nThe project is ready for README ",
    "documentation and blog writing.\n"
  )
)