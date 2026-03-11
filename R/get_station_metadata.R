## ---- Station Metadata ----

#' Get Station Metadata
#'
#' @description Retrieves the available variables for a specific NDBC buoy station 
#'   and calculates the minimum and maximum dates of valid observations within 
#'   the available real-time dataset.
#'
#' @param station_id A string representing the 5-character NDBC station identifier (e.g., "44013").
#'
#' @return A tibble containing `varname`, `min_date`, and `max_date`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get variable availability and date ranges for station 44013
#' get_station_metadata("44013")
#' }
get_station_metadata <- function(station_id) {
  
  # 1. Construct URL
  base_url <- "https://www.ndbc.noaa.gov/data/realtime2/"
  data_url <- paste0(base_url, station_id, ".txt")
  
  # 2. Check existence & handle potential download errors
  header_line <- tryCatch({
    suppressWarnings(readLines(data_url, n = 1))
  }, error = function(e) {
    stop("URL download failed. Please check your internet connection or firewall.", call. = FALSE)
  })
  
  # NDBC returns HTML containing "404 Not Found" when a station ID is invalid
  if (length(header_line) == 0 || grepl("404 Not Found|html", header_line[1], ignore.case = TRUE)) {
    stop(sprintf("Station_id '%s' not found or real-time data is unavailable.", station_id), call. = FALSE)
  }
  
  # 3. Parse Header to establish column names
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
  
  # 5. Restructure and calculate metadata summaries
  metadata_summary <- raw_data |>
    dplyr::mutate(
      date = as.Date(sprintf("%04d-%02d-%02d", as.integer(YY), as.integer(MM), as.integer(DD)))
    ) |>
    # Remove time components as we are calculating daily ranges
    dplyr::select(-YY, -MM, -DD, -hh, -mm) |>
    tidyr::pivot_longer(
      cols = -date,
      names_to = "varname",
      values_to = "value"
    ) |>
    # Filter out NAs to ensure we only get date ranges for ACTUAL recorded values
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(varname) |>
    dplyr::summarize(
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE),
      .groups = "drop" # Drop grouping to return a clean tibble
    )
  
  return(metadata_summary)
}