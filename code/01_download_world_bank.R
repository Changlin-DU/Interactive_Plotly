# ============================================================
# Download raw data from the World Bank Indicators API
# Project: Economic Growth and Carbon Emissions Decoupling
# ============================================================

# This script downloads:
# 1. World Bank country and economy metadata
# 2. Real GDP per capita
# 3. CO2 emissions per capita
# 4. Total population
# 5. Renewable electricity output
# 6. Official indicator metadata
# 7. Download and data-quality records
#
# API documentation:
# https://datahelpdesk.worldbank.org/knowledgebase/articles/889392
#
# No API key is required.


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
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "httr2",
  "jsonlite",
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
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(httr2)
  library(jsonlite)
  library(janitor)
})


# 3. Set download parameters ----------------------------------------------

download_start_year <- 1990L
download_end_year <- 2025L
observations_per_page <- 20000L

download_time_utc <- format(
  Sys.time(),
  tz = "UTC",
  format = "%Y-%m-%d %H:%M:%S UTC"
)

message(
  "Requested period: ",
  download_start_year,
  "–",
  download_end_year
)


# 4. Define the World Bank indicators -------------------------------------

indicators <- tibble::tribble(
  ~indicator_code,         ~short_name,                   ~expected_indicator,
  "NY.GDP.PCAP.PP.KD",     "gdp_per_capita_ppp",          "GDP per capita, PPP (constant 2021 international $)",
  "EN.GHG.CO2.PC.CE.AR5",  "co2_per_capita",              "Carbon dioxide emissions excluding LULUCF per capita (t CO2e/capita)",
  "SP.POP.TOTL",           "population",                  "Population, total",
  "EG.ELC.RNEW.ZS",        "renewable_electricity_share", "Renewable electricity output (% of total electricity output)"
)

print(indicators)


# 5. Define API helper functions ------------------------------------------

perform_world_bank_request <- function(request_object) {
  
  response <- request_object |>
    httr2::req_user_agent(
      "AEDS6400-BlogPost7-Interactive-Plotly"
    ) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(
      max_tries = 5,
      backoff = function(tries) min(2 ^ tries, 30)
    ) |>
    httr2::req_perform()
  
  response_status <- httr2::resp_status(response)
  
  if (response_status != 200) {
    stop(
      "World Bank API request failed with HTTP status ",
      response_status
    )
  }
  
  response
}


parse_world_bank_response <- function(response) {
  
  response_text <- httr2::resp_body_string(response)
  
  parsed <- jsonlite::fromJSON(
    response_text,
    flatten = TRUE
  )
  
  if (length(parsed) < 2 || is.null(parsed[[2]])) {
    stop(
      "The World Bank API returned no observation data. ",
      "Check whether the requested indicator code is current."
    )
  }
  
  list(
    page_information = parsed[[1]],
    observations = tibble::as_tibble(parsed[[2]]),
    response_text = response_text
  )
}


download_indicator_page <- function(
    indicator_code,
    page_number,
    start_year,
    end_year,
    per_page
) {
  
  endpoint <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/",
    indicator_code
  )
  
  request_object <- httr2::request(endpoint) |>
    httr2::req_url_query(
      format = "json",
      date = paste0(start_year, ":", end_year),
      per_page = per_page,
      page = page_number
    )
  
  response <- perform_world_bank_request(
    request_object
  )
  
  parse_world_bank_response(
    response
  )
}


safe_year_range <- function(year, value) {
  
  valid_years <- suppressWarnings(
    as.integer(year[!is.na(value)])
  )
  
  valid_years <- valid_years[
    !is.na(valid_years)
  ]
  
  if (length(valid_years) == 0) {
    return(
      c(
        NA_integer_,
        NA_integer_
      )
    )
  }
  
  c(
    min(valid_years),
    max(valid_years)
  )
}


# 6. Define the indicator-download function -------------------------------

download_indicator <- function(
    indicator_code,
    short_name,
    expected_indicator
) {
  
  message("\nDownloading indicator: ", indicator_code)
  message("Expected definition: ", expected_indicator)
  
  first_page <- download_indicator_page(
    indicator_code = indicator_code,
    page_number = 1L,
    start_year = download_start_year,
    end_year = download_end_year,
    per_page = observations_per_page
  )
  
  total_pages <- as.integer(
    first_page$page_information$pages[[1]]
  )
  
  message("API pages: ", total_pages)
  
  if (is.na(total_pages) || total_pages < 1L) {
    stop(
      "Invalid page count returned for indicator ",
      indicator_code
    )
  }
  
  if (total_pages == 1L) {
    
    indicator_data <- first_page$observations
    
  } else {
    
    remaining_pages <- purrr::map(
      2:total_pages,
      function(page_number) {
        
        Sys.sleep(0.2)
        
        download_indicator_page(
          indicator_code = indicator_code,
          page_number = page_number,
          start_year = download_start_year,
          end_year = download_end_year,
          per_page = observations_per_page
        )$observations
      }
    )
    
    indicator_data <- dplyr::bind_rows(
      c(
        list(first_page$observations),
        remaining_pages
      )
    )
  }
  
  indicator_data <- indicator_data |>
    janitor::clean_names() |>
    dplyr::mutate(
      indicator_code_requested = indicator_code,
      downloaded_at_utc = download_time_utc
    )
  
  required_columns <- c(
    "country_id",
    "countryiso3code",
    "date",
    "value"
  )
  
  absent_columns <- setdiff(
    required_columns,
    names(indicator_data)
  )
  
  if (length(absent_columns) > 0) {
    stop(
      "The downloaded data for ",
      indicator_code,
      " are missing expected columns: ",
      paste(absent_columns, collapse = ", ")
    )
  }
  
  output_file <- file.path(
    "data",
    "raw",
    paste0(short_name, ".csv")
  )
  
  readr::write_csv(
    indicator_data,
    output_file,
    na = ""
  )
  
  year_range <- safe_year_range(
    year = indicator_data$date,
    value = indicator_data$value
  )
  
  if (
    "indicator_value" %in% names(indicator_data) &&
    any(!is.na(indicator_data$indicator_value))
  ) {
    
    actual_indicator_name <- indicator_data$indicator_value |>
      stats::na.omit() |>
      unique() |>
      first()
    
  } else {
    
    actual_indicator_name <- expected_indicator
  }
  
  manifest_row <- tibble::tibble(
    indicator_code = indicator_code,
    short_name = short_name,
    expected_indicator = expected_indicator,
    api_indicator_name = actual_indicator_name,
    requested_start_year = download_start_year,
    requested_end_year = download_end_year,
    earliest_nonmissing_year = year_range[[1]],
    latest_nonmissing_year = year_range[[2]],
    total_rows = nrow(indicator_data),
    nonmissing_values = sum(!is.na(indicator_data$value)),
    raw_file = output_file,
    downloaded_at_utc = download_time_utc
  )
  
  message("Saved: ", output_file)
  message("Rows: ", nrow(indicator_data))
  message(
    "Nonmissing observations: ",
    sum(!is.na(indicator_data$value))
  )
  message(
    "Observed year range: ",
    year_range[[1]],
    "–",
    year_range[[2]]
  )
  
  list(
    data = indicator_data,
    manifest = manifest_row
  )
}


# 7. Download country metadata --------------------------------------------

message("\nDownloading World Bank country metadata...")

country_request <- httr2::request(
  "https://api.worldbank.org/v2/country"
) |>
  httr2::req_url_query(
    format = "json",
    per_page = 400,
    page = 1
  )

country_response <- perform_world_bank_request(
  country_request
)

country_parsed <- parse_world_bank_response(
  country_response
)

country_metadata <- country_parsed$observations |>
  janitor::clean_names() |>
  dplyr::mutate(
    downloaded_at_utc = download_time_utc
  )

readr::write_csv(
  country_metadata,
  "data/raw/country_metadata.csv",
  na = ""
)

message(
  "Saved country metadata: data/raw/country_metadata.csv"
)

message(
  "Country and aggregate records: ",
  nrow(country_metadata)
)


# 8. Download all indicator observations ----------------------------------

download_results <- purrr::pmap(
  indicators,
  function(
    indicator_code,
    short_name,
    expected_indicator
  ) {
    
    result <- download_indicator(
      indicator_code = indicator_code,
      short_name = short_name,
      expected_indicator = expected_indicator
    )
    
    Sys.sleep(0.3)
    
    result
  }
)


# 9. Save official indicator metadata -------------------------------------

message("\nDownloading official indicator metadata...")

for (indicator_code in indicators$indicator_code) {
  
  metadata_endpoint <- paste0(
    "https://api.worldbank.org/v2/indicator/",
    indicator_code
  )
  
  metadata_request <- httr2::request(
    metadata_endpoint
  ) |>
    httr2::req_url_query(
      format = "json"
    )
  
  metadata_response <- perform_world_bank_request(
    metadata_request
  )
  
  metadata_text <- httr2::resp_body_string(
    metadata_response
  )
  
  metadata_file_name <- paste0(
    "indicator_metadata_",
    stringr::str_to_lower(
      stringr::str_replace_all(
        indicator_code,
        "[^A-Za-z0-9]+",
        "_"
      )
    ),
    ".json"
  )
  
  metadata_output_file <- file.path(
    "data",
    "raw",
    metadata_file_name
  )
  
  writeLines(
    metadata_text,
    con = metadata_output_file,
    useBytes = TRUE
  )
  
  message(
    "Saved: ",
    metadata_output_file
  )
  
  Sys.sleep(0.2)
}


# 10. Create and save the download manifest -------------------------------

download_manifest <- purrr::map_dfr(
  download_results,
  "manifest"
) |>
  dplyr::arrange(
    indicator_code
  )

readr::write_csv(
  download_manifest,
  "data/raw/download_manifest.csv",
  na = ""
)


# 11. Check the true World Bank observation key ---------------------------

# country_id is the World Bank's identifying code returned inside the
# country object. Unlike countryiso3code, it is available for World Bank
# aggregates and special economies. It is therefore the appropriate key
# for checking uniqueness in the unfiltered raw API response.

quality_check <- purrr::map_dfr(
  download_results,
  function(result) {
    
    data <- result$data
    
    duplicate_country_year_pairs <- data |>
      dplyr::count(
        country_id,
        date,
        name = "number_of_rows"
      ) |>
      dplyr::filter(
        number_of_rows > 1L
      ) |>
      nrow()
    
    missing_country_id_rows <- sum(
      is.na(data$country_id) |
        data$country_id == ""
    )
    
    missing_iso3_rows <- sum(
      is.na(data$countryiso3code) |
        data$countryiso3code == ""
    )
    
    tibble::tibble(
      indicator_code = first(
        data$indicator_code_requested
      ),
      duplicate_country_id_year_pairs =
        duplicate_country_year_pairs,
      missing_country_id_rows =
        missing_country_id_rows,
      missing_iso3_rows =
        missing_iso3_rows
    )
  }
)

readr::write_csv(
  quality_check,
  "data/raw/duplicate_check.csv",
  na = ""
)

if (
  any(
    quality_check$duplicate_country_id_year_pairs > 0
  )
) {
  
  warning(
    "At least one indicator contains genuine duplicate ",
    "country_id-year observations. Review ",
    "data/raw/duplicate_check.csv."
  )
  
} else {
  
  message(
    "No genuine duplicate country_id-year pairs were detected."
  )
}


# 12. Validate all expected output files ----------------------------------

expected_files <- c(
  "data/raw/country_metadata.csv",
  "data/raw/gdp_per_capita_ppp.csv",
  "data/raw/co2_per_capita.csv",
  "data/raw/population.csv",
  "data/raw/renewable_electricity_share.csv",
  "data/raw/download_manifest.csv",
  "data/raw/duplicate_check.csv"
)

missing_output_files <- expected_files[
  !file.exists(expected_files)
]

if (length(missing_output_files) > 0) {
  stop(
    "The following expected files were not created: ",
    paste(missing_output_files, collapse = ", ")
  )
}

file_sizes <- file.info(
  expected_files
)$size

empty_output_files <- expected_files[
  is.na(file_sizes) |
    file_sizes == 0
]

if (length(empty_output_files) > 0) {
  stop(
    "The following output files are empty: ",
    paste(empty_output_files, collapse = ", ")
  )
}


# 13. Print download summary ----------------------------------------------

cat("\n")
cat("============================================================\n")
cat("STEP 2 COMPLETED: WORLD BANK DATA DOWNLOADED\n")
cat("============================================================\n")

cat(
  "Download time:     ",
  download_time_utc,
  "\n",
  sep = ""
)

cat(
  "Requested years:   ",
  download_start_year,
  "–",
  download_end_year,
  "\n",
  sep = ""
)

cat(
  "Indicators:        ",
  nrow(indicators),
  "\n",
  sep = ""
)

cat(
  "Country records:   ",
  nrow(country_metadata),
  "\n",
  sep = ""
)

cat(
  "Manifest:          data/raw/download_manifest.csv\n"
)

cat(
  "Quality check:     data/raw/duplicate_check.csv\n"
)

cat("============================================================\n\n")


cat("Indicator coverage:\n")

print(
  download_manifest |>
    dplyr::select(
      indicator_code,
      api_indicator_name,
      earliest_nonmissing_year,
      latest_nonmissing_year,
      total_rows,
      nonmissing_values
    ),
  width = Inf
)


cat("\nData-quality check:\n")

print(
  quality_check,
  width = Inf
)


cat("\nRaw files created:\n")

created_raw_files <- list.files(
  "data/raw",
  full.names = TRUE
)

cat(
  paste0(" - ", created_raw_files),
  sep = "\n"
)