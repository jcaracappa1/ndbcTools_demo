## ---- Data Import ----

#' Pull NDBC Buoy Data
#'
#' @description Retrieves and parses standard meteorological data for a specific
#'   National Data Buoy Center (NDBC) station. The function downloads the raw text
#'   data, formats the timestamps, and pivots the results into a tidy long format.
#'
#' @param station_id A string representing the 5-character NDBC station identifier (e.g., "44013").
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
#' # Pull specific variables within a date range
#' pull_buoy_data(
#'   station_id = "44013",
#'   start_date = Sys.Date() - 7,
#'   end_date = Sys.Date(),
#'   var_name = c("WSPD", "WVHT")
#' )
#' }
pull_buoy_data <- function(station_id, start_date = NULL, end_date = NULL, var_name = NULL) {

  # 1. Construct URL
  base_url <- "https://www.ndbc.noaa.gov/data/realtime2/"
  data_url <- paste0(base_url, station_id, ".txt")

  # 2. Check existence & handle potential download errors
  # Attempt to read the first line to test connection and verify 404 status
  header_line <- tryCatch({
    suppressWarnings(readLines(data_url, n = 1))
  }, error = function(e) {
    stop("URL download failed. Please check your internet connection or firewall.", call. = FALSE)
  })

  # NDBC returns HTML containing "404 Not Found" when a station ID is invalid
  if (length(header_line) == 0 || grepl("404 Not Found|html", header_line[1], ignore.case = TRUE)) {
    stop(sprintf("Station_id '%s' doesn't exist or real-time data is unavailable.", station_id), call. = FALSE)
  }

  # 3. Parse Header to establish column names
  # NDBC standard format separates by whitespace and prefixes the first column with '#'
  col_names <- unlist(strsplit(trimws(header_line), "\\s+"))
  col_names[1] <- gsub("^#", "", col_names[1])

  # 4. Download and parse data into a dataframe
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

  # 5. Restructure into the requested tidy long format
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

  # 6. Error Handle & Filter: var_name
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

  # 7. Filter: start_date
  if (!is.null(start_date)) {
    processed_data <- processed_data |>
      dplyr::filter(date >= as.Date(start_date))
  }

  # 8. Filter: end_date
  if (!is.null(end_date)) {
    processed_data <- processed_data |>
      dplyr::filter(date <= as.Date(end_date))
  }

  return(processed_data)
}
