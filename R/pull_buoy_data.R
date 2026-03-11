## ---- Internal Helpers ----

#' Retrieve Correct Case for Station ID
#'
#' @description Fetches the NDBC directory index and performs a case-insensitive
#'   match to return the exact string expected by the server. This prevents 404
#'   errors caused by case mismatches (e.g., "bluv2" vs "BLUV2").
#'
#' @param station_id A string representing the user-provided station ID.
#'
#' @return A string with the correctly cased station ID.
#' @noRd
.get_true_station_id <- function(station_id) {
  base_url <- "https://www.ndbc.noaa.gov/data/realtime2/"
  
  # Fetch the HTML index of the realtime directory
  index_lines <- tryCatch({
    suppressWarnings(readLines(base_url, warn = FALSE))
  }, error = function(e) {
    stop("Failed to connect to NDBC directory index. Please check your internet connection.", call. = FALSE)
  })
  
  # Combine lines into a single string for regex extraction
  index_html <- paste(index_lines, collapse = " ")
  
  # Extract all file links ending in .txt
  file_matches <- stringr::str_extract_all(index_html, 'href="[^"]+\\.txt"')[[1]]
  
  # Strip the HTML syntax and file extension to isolate the available station IDs
  available_ids <- file_matches |>
    stringr::str_remove_all('href="|"|\\.txt')
  
  # Perform a case-insensitive match
  match_idx <- match(tolower(station_id), tolower(available_ids))
  
  if (is.na(match_idx)) {
    stop(sprintf("Station ID '%s' not found in the active NDBC realtime database.", station_id), call. = FALSE)
  }
  
  # Return the correctly cased ID straight from the NDBC index
  return(available_ids[match_idx])
}

## ---- Data Import ----

#' Pull NDBC Buoy Data
#'
#' @title Pull NDBC Buoy Data
#' @description Retrieves and parses standard meteorological data for a specific
#'   National Data Buoy Center (NDBC) station. The function validates the station
#'   against the NDBC index (resolving case sensitivity), downloads the raw text
#'   data, formats the timestamps, and pivots the results into a tidy long format.
#'
#' @param station_id A string representing the 5-character NDBC station identifier (e.g., "44013" or "BLUV2").
#' @param start_date A string or Date object for the start of the filter range (e.g., "2023-01-01"). Optional.
#' @param end_date A string or Date object for the end of the filter range (e.g., "2023-12-31"). Optional.
#' @param var_name A character vector of variables to return (e.g., c("WSPD", "WVHT")). Optional.
#'
#' @return A tidy tibble (data frame) in long format containing `date`, `time`, `varname`, and `value`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull all available variables for station 44013
#' pull_buoy_data("44013")
#'
#' # Case-insensitive ID pull for specific variables within a date range
#' pull_buoy_data(
#'   station_id = "bluv2",
#'   start_date = Sys.Date() - 7,
#'   end_date = Sys.Date(),
#'   var_name = c("WSPD", "WVHT")
#' )
#' }
pull_buoy_data <- function(station_id, start_date = NULL, end_date = NULL, var_name = NULL) {
  
  # 1. Validate and get the correctly cased station ID
  true_station_id <- .get_true_station_id(station_id)
  
  # 2. Construct URL
  base_url <- "https://www.ndbc.noaa.gov/data/realtime2/"
  data_url <- paste0(base_url, true_station_id, ".txt")
  
  # 3. Fetch header and check existence (fallback check)
  # Read the first line to establish column names and verify connection
  header_line <- tryCatch({
    suppressWarnings(readLines(data_url, n = 1))
  }, error = function(e) {
    stop("URL download failed. Please check your internet connection or firewall.", call. = FALSE)
  })
  
  # Fallback: Check if NDBC returned a hidden HTML 404 despite passing the index check
  if (length(header_line) == 0 || grepl("404 Not Found|html", header_line[1], ignore.case = TRUE)) {
    stop(sprintf("Data for station_id '%s' is currently unavailable.", true_station_id), call. = FALSE)
  }
  
  # 4. Parse Header to establish column names
  # NDBC standard format separates by whitespace and prefixes the first column with '#'
  col_names <- unlist(strsplit(trimws(header_line), "\\s+"))
  col_names[1] <- gsub("^#", "", col_names[1])
  
  # 5. Download and parse data into a dataframe
  raw_data <- tryCatch({
    readr::read_table(
      data_url,
      col_names = col_names,
      skip = 2, # Skip header and unit rows
      show_col_types = FALSE,
      na = c("MM", "99.0", "99.00", "999", "9999") # Standard NDBC missing data flags
    )
  }, error = function(e) {
    stop("Failed to parse the downloaded tabular data.", call. = FALSE)
  })
  
  # 6. Restructure into the requested tidy long format
  processed_data <- raw_data |>
    dplyr::mutate(
      # Combine components into standardized date and time strings
      date_string = sprintf("%04d-%02d-%02d", as.integer(YY), as.integer(MM), as.integer(DD)),
      time = sprintf("%02d:%02d:00", as.integer(hh), as.integer(mm)),
      date = as.Date(date_string)
    ) |>
    # Remove original separate timestamp columns
    dplyr::select(-YY, -MM, -DD, -hh, -mm, -date_string) |>
    # Pivot to long format: (date, time, varname, value)
    tidyr::pivot_longer(
      cols = -c(date, time),
      names_to = "varname",
      values_to = "value"
    )
  
  # 7. Error Handle & Filter: var_name
  if (!is.null(var_name)) {
    # Check if any requested variables are entirely missing from the dataset
    missing_vars <- setdiff(var_name, unique(processed_data$varname))
    
    if (length(missing_vars) > 0) {
      stop(
        sprintf("The following var_name(s) were not found in the data: %s",
                paste(missing_vars, collapse = ", ")),
        call. = FALSE
      )
    }
    
    processed_data <- processed_data |>
      dplyr::filter(varname %in% var_name)
  }
  
  # 8. Filter: start_date
  if (!is.null(start_date)) {
    processed_data <- processed_data |>
      dplyr::filter(date >= as.Date(start_date))
  }
  
  # 9. Filter: end_date
  if (!is.null(end_date)) {
    processed_data <- processed_data |>
      dplyr::filter(date <= as.Date(end_date))
  }
  
  return(processed_data)
}