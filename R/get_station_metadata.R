## ---- Station Metadata ----

#' Get Station Metadata
#'
#' @description Retrieves the available variables for a specific NDBC buoy station 
#'   and calculates the minimum and maximum dates of valid observations within 
#'   the available real-time dataset.
#'
#' @param station_id A string representing the 5-character NDBC station identifier (e.g., "44013").
#'
#' @return A tibble containing `varname`, `description`, `min_date`, and `max_date`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get variable availability and date ranges for station 44013
#' get_station_metadata("44013")
#' }
get_station_metadata <- function(station_id) {
  
  # 1. Fetch the active real-time index to find the official ID casing
  base_url <- "https://www.ndbc.noaa.gov/data/realtime2/"
  
  index_html <- tryCatch({
    suppressWarnings(readLines(base_url))
  }, error = function(e) {
    stop("Failed to access NDBC index. Please check your internet connection.", call. = FALSE)
  })
  
  # Extract all available station IDs (matching href="ID.txt")
  extracted_matches <- stringr::str_match(index_html, "href=\"([^\"]+)\\.txt\"")
  active_stations <- extracted_matches[!is.na(extracted_matches[, 2]), 2]
  
  # Case-insensitive match against the active index
  match_idx <- which(tolower(active_stations) == tolower(station_id))
  
  if (length(match_idx) == 0) {
    stop(sprintf("Station_id '%s' not found or real-time data is unavailable.", station_id), call. = FALSE)
  }
  
  official_station_id <- active_stations[match_idx[1]]
  
  # 2. Construct URL and fetch header
  data_url <- paste0(base_url, official_station_id, ".txt")
  
  header_line <- tryCatch({
    suppressWarnings(readLines(data_url, n = 1))
  }, error = function(e) {
    stop("URL download failed. Please check your internet connection or firewall.", call. = FALSE)
  })
  
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
  
  # 5. Define NDBC Standard Variable Descriptions Lookup
  var_descriptions <- tibble::tribble(
    ~varname, ~description,
    "WDIR",   "Wind Direction",
    "WSPD",   "Wind Speed",
    "GST",    "Wind Gust",
    "WVHT",   "Significant Wave Height",
    "DPD",    "Dominant Wave Period",
    "APD",    "Average Wave Period",
    "MWD",    "Mean Wave Direction",
    "PRES",   "Atmospheric Pressure",
    "ATMP",   "Air Temperature",
    "WTMP",   "Water Temperature",
    "DEWP",   "Dew Point Temperature",
    "VIS",    "Visibility",
    "PTDY",   "Pressure Tendency",
    "TIDE",   "Water Level"
  )
  
  # 6. Restructure and calculate metadata summaries
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
    ) |>
    # Attach the descriptive names
    dplyr::left_join(var_descriptions, by = "varname") |>
    # Reorder columns for readability
    dplyr::relocate(description, .after = varname)
  
  return(metadata_summary)
}