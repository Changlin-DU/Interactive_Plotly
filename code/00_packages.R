# ============================================================
# Install and load required packages
# Project: Economic Growth and Carbon Emissions Decoupling
# ============================================================

# This script:
# 1. Confirms that R is running from the project root
# 2. Installs any missing packages
# 3. Loads all packages required for the analysis
# 4. Tests the World Bank Indicators API connection
# 5. Prints package versions for reproducibility


# 1. Confirm the working directory -----------------------------------------

expected_project <- "blogpost7_Interactive_Plotly"
current_project <- basename(normalizePath(getwd()))

if (current_project != expected_project) {
  stop(
    "The project is not running from the expected directory.\n",
    "Expected: ", expected_project, "\n",
    "Current:  ", current_project, "\n\n",
    "Open blogpost7_Interactive_Plotly.Rproj and run this script again."
  )
}

message("Project directory confirmed: ", normalizePath(getwd()))


# 2. Set general options ---------------------------------------------------

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 300,
  scipen = 999,
  digits = 4
)


# 3. List required packages ------------------------------------------------

required_packages <- c(
  # Data manipulation
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  
  # API and JSON access
  "httr2",
  "jsonlite",
  
  # Country classifications
  "countrycode",
  
  # Visualization
  "ggplot2",
  "plotly",
  "htmlwidgets",
  "scales",
  
  # Project organization and cleaning
  "here",
  "janitor"
)


# 4. Install missing packages ---------------------------------------------

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  
  install.packages(
    missing_packages,
    dependencies = NA
  )
} else {
  message("All required packages are already installed.")
}


# 5. Confirm installation -------------------------------------------------

installed_after_setup <- rownames(installed.packages())
failed_packages <- setdiff(required_packages, installed_after_setup)

if (length(failed_packages) > 0) {
  stop(
    "The following packages were not installed successfully: ",
    paste(failed_packages, collapse = ", "),
    "\nReview the installation messages above and try again."
  )
}


# 6. Load packages ---------------------------------------------------------

invisible(
  lapply(
    required_packages,
    function(package_name) {
      suppressPackageStartupMessages(
        library(
          package_name,
          character.only = TRUE
        )
      )
    }
  )
)

message("All required packages loaded successfully.")


# 7. Check the project folders --------------------------------------------

required_directories <- c(
  "code",
  "data/raw",
  "data/processed",
  "results/figures",
  "results/interactive",
  "results/tables"
)

missing_directories <- required_directories[
  !dir.exists(required_directories)
]

if (length(missing_directories) > 0) {
  stop(
    "The following project directories are missing: ",
    paste(missing_directories, collapse = ", ")
  )
} else {
  message("All required project directories are present.")
}


# 8. Test the World Bank API ----------------------------------------------

world_bank_test_url <- paste0(
  "https://api.worldbank.org/v2/country",
  "?format=json",
  "&per_page=1"
)

api_test <- tryCatch(
  {
    response <- httr2::request(world_bank_test_url) |>
      httr2::req_user_agent(
        "AEDS6400-BlogPost7-Interactive-Plotly"
      ) |>
      httr2::req_timeout(seconds = 30) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()
    
    list(
      success = httr2::resp_status(response) == 200,
      status = httr2::resp_status(response),
      error = NA_character_
    )
  },
  error = function(error_condition) {
    list(
      success = FALSE,
      status = NA_integer_,
      error = conditionMessage(error_condition)
    )
  }
)

if (isTRUE(api_test$success)) {
  message(
    "World Bank API connection successful. HTTP status: ",
    api_test$status
  )
} else {
  warning(
    "World Bank API connection test failed.\n",
    "HTTP status: ", api_test$status, "\n",
    "Message: ", api_test$error
  )
}


# 9. Create a package-version table ---------------------------------------

package_versions <- tibble::tibble(
  package = required_packages,
  version = vapply(
    required_packages,
    function(package_name) {
      as.character(packageVersion(package_name))
    },
    character(1)
  )
) |>
  dplyr::arrange(package)

readr::write_csv(
  package_versions,
  "results/tables/package_versions.csv"
)


# 10. Print verification summary ------------------------------------------

cat("\n")
cat("============================================================\n")
cat("STEP 1 COMPLETED\n")
cat("============================================================\n")
cat("Project:          ", current_project, "\n", sep = "")
cat("R version:        ", R.version.string, "\n", sep = "")
cat("Required packages:", length(required_packages), "\n")
cat("API connected:    ", api_test$success, "\n", sep = "")
cat(
  "Version table:   results/tables/package_versions.csv\n"
)
cat("============================================================\n\n")

print(package_versions)

cat("\nSession information:\n")
print(sessionInfo())