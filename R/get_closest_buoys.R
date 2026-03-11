## ---- Station Location Search ----

#' Find Closest NOAA Buoys
#'
#' @description Retrieves coordinates of all listed NDBC buoys and calculates the 
#'   geographic distance (using the Haversine formula) to find the closest `n` 
#'   stations within a specified maximum distance.
#'
#' @param lat A numeric vector representing the target latitude(s) in decimal degrees.
#' @param lon A numeric vector representing the target longitude(s) in decimal degrees.
#' @param max_dist A numeric value for the maximum search distance in kilometers.
#' @param n An integer specifying the maximum number of closest stations to return.
#'
#' @return A tibble containing `original_lat`, `original_lon`, `station_id`, 
#'   `station_lat`, `station_lon`, and `distance_km`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the 5 closest buoys within 100km of Narragansett, RI (41.43, -71.46)
#' get_closest_buoys(lat = 41.43, lon = -71.46, max_dist = 100, n = 5)
#' 
#' # Vectorized example: Find closest buoys for multiple locations
#' get_closest_buoys(lat = c(41.43, 42.36), lon = c(-71.46, -71.06), max_dist = 50, n = 2)
#' }
get_closest_buoys <- function(lat, lon, max_dist, n) {
  
  if (length(lat) != length(lon)) {
    stop("The 'lat' and 'lon' vectors must be of equal length.", call. = FALSE)
  }
  
  # 1. Fetch Station Table
  # NDBC maintains a pipe-delimited master list of all stations
  station_url <- "https://www.ndbc.noaa.gov/data/stations/station_table.txt"
  
  station_data <- tryCatch({
    readr::read_delim(
      station_url,
      delim = "|",
      show_col_types = FALSE,
      progress = FALSE
    )
  }, error = function(e) {
    stop("Failed to download station data. Please check your internet connection.", call. = FALSE)
  })
  
  # Clean column names (remove whitespace and leading '#')
  colnames(station_data) <- trimws(colnames(station_data))
  colnames(station_data) <- gsub("^#\\s*", "", colnames(station_data))
  
  # 2. Ensure Expected Columns Exist
  if (!all(c("STATION_ID", "LOCATION") %in% colnames(station_data))) {
    stop("Station data format has changed. Could not locate STATION_ID or LOCATION columns.", call. = FALSE)
  }
  
  # 3. Clean Data and Parse Coordinates
  # Extract lat/lon from the LOCATION string: "32.400 N 120.582 W (...)"
  parsed_locs <- stringr::str_match(station_data$LOCATION, "^([0-9.]+)\\s+([NS])\\s+([0-9.]+)\\s+([EW])")
  
  cleaned_stations <- station_data |>
    dplyr::mutate(
      station_id = as.character(STATION_ID),
      station_lat = as.numeric(parsed_locs[, 2]) * dplyr::if_else(parsed_locs[, 3] == "S", -1, 1),
      station_lon = as.numeric(parsed_locs[, 4]) * dplyr::if_else(parsed_locs[, 5] == "W", -1, 1)
    ) |>
    # Remove any stations with missing coordinate data
    dplyr::filter(!is.na(station_lat) & !is.na(station_lon))
  
  # 4. Calculate Distances
  # Define a simple Haversine calculation to avoid adding spatial package dependencies
  deg2rad <- function(deg) deg * pi / 180
  R <- 6371 # Earth's mean radius in km
  
  # Iterate over each coordinate pair
  results <- purrr::map2_dfr(lat, lon, function(current_lat, current_lon) {
    
    processed_stations <- cleaned_stations |>
      # Calculate Haversine distance for the current coordinate pair
      dplyr::mutate(
        dlat = deg2rad(station_lat - current_lat),
        dlon = deg2rad(station_lon - current_lon),
        a = sin(dlat / 2)^2 + cos(deg2rad(current_lat)) * cos(deg2rad(station_lat)) * sin(dlon / 2)^2,
        c = 2 * atan2(sqrt(a), sqrt(1 - a)),
        distance_km = R * c,
        original_lat = current_lat,
        original_lon = current_lon
      ) |>
      # Filter by the user's maximum distance threshold
      dplyr::filter(distance_km <= max_dist) |>
      # Sort closest to furthest
      dplyr::arrange(distance_km) |>
      # Isolate the requested output columns
      dplyr::select(
        original_lat, 
        original_lon, 
        station_id, 
        station_lat, 
        station_lon, 
        distance_km
      )
    
    # 5. Error Handle: No stations fit criteria for this specific coordinate
    if (nrow(processed_stations) == 0) {
      warning(
        sprintf("No stations fit criteria within %s km of coordinates (%s, %s).", 
                max_dist, current_lat, current_lon), 
        call. = FALSE
      )
      return(processed_stations) # Returns empty tibble with correct columns
    }
    
    # 6. Limit to Top 'n'
    utils::head(processed_stations, n)
  })
  
  return(results)
}