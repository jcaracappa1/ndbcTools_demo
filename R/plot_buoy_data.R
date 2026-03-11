## ---- Plotting Functions ----

#' Plot NDBC Buoy Data
#'
#' @description Retrieves NDBC buoy data via \code{pull_buoy_data()} and creates a 
#'   faceted timeseries plot. Supports temporal aggregation (daily, monthly, yearly) 
#'   and calculates standard errors for the aggregated intervals to display as a 
#'   shaded region.
#'
#' @param station_id A string representing the 5-character NDBC station identifier.
#' @param start_date A string or Date object for the start of the filter range. Optional.
#' @param end_date A string or Date object for the end of the filter range. Optional.
#' @param var_name A character vector of variables to return. Optional.
#' @param agg_time A string indicating the aggregation level: "day", "month", "year", 
#'   or NULL for no aggregation.
#' @param write A logical indicating whether to save the plot to disk. Defaults to FALSE.
#' @param file_name A string representing the file path to save the plot if write is TRUE.
#'
#' @return A \code{ggplot2} object containing the timeseries plot.
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot raw data (no aggregation)
#' plot_buoy_data("bluv2", start_date = Sys.Date() - 7)
#' 
#' # Plot daily aggregated data and save to file
#' plot_buoy_data(
#'   station_id = "bluv2",
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31",
#'   var_name = c("WSPD", "WVHT"),
#'   agg_time = "day",
#'   write = TRUE,
#'   file_name = "bluv2_daily_summary.png"
#' )
#' }
plot_buoy_data <- function(station_id, start_date = NULL, end_date = NULL, 
                           var_name = NULL, agg_time = NULL, 
                           write = FALSE, file_name = "buoy_plot.png") {
  
  # 1. Retrieve Data
  # Errors from pull_buoy_data() will bubble up natively to the user
  buoy_data <- pull_buoy_data(
    station_id = station_id,
    start_date = start_date,
    end_date = end_date,
    var_name = var_name
  )
  
  if (nrow(buoy_data) == 0) {
    stop("No data available to plot for the given parameters.", call. = FALSE)
  }
  
  # 2. Validate Aggregation Argument
  valid_agg <- c("day", "month", "year")
  if (!is.null(agg_time) && !agg_time %in% valid_agg) {
    stop("Aggregation not possible. agg_time must be 'day', 'month', 'year', or NULL.", call. = FALSE)
  }
  
  # 3. Calculate overall means for the horizontal dashed line (across entire timeseries)
  overall_means <- buoy_data |>
    dplyr::group_by(varname) |>
    dplyr::summarize(overall_mean = mean(value, na.rm = TRUE), .groups = "drop")
  
  # 4. Perform Temporal Aggregation
  if (is.null(agg_time)) {
    # No aggregation: create full datetime sequence for precise plotting
    plot_data <- buoy_data |>
      dplyr::mutate(
        plot_time = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        mean_val = value # Map raw value to mean_val to simplify ggplot aesthetics below
      )
  } else {
    # Time-based aggregation
    plot_data <- buoy_data |>
      dplyr::mutate(
        plot_time = dplyr::case_when(
          agg_time == "day" ~ as.Date(date),
          agg_time == "month" ~ as.Date(format(date, "%Y-%m-01")),
          agg_time == "year" ~ as.Date(format(date, "%Y-01-01"))
        )
      ) |>
      dplyr::group_by(varname, plot_time) |>
      dplyr::summarize(
        n = sum(!is.na(value)),
        mean_val = mean(value, na.rm = TRUE),
        sd_val = sd(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        # Calculate Standard Error (SE = SD / sqrt(n))
        se_val = dplyr::if_else(n > 1, sd_val / sqrt(n), 0)
      )
  }
  
  # 5. Build ggplot Object
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = plot_time, y = mean_val))
  
  # Add shaded region (ribbon) only if aggregation is performed and we have SE
  if (!is.null(agg_time)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = mean_val - se_val, ymax = mean_val + se_val, fill = varname),
        alpha = 0.3,
        show.legend = FALSE
      )
  }
  
  # Add the timeseries line, the overall mean line, and formatting facets
  p <- p +
    ggplot2::geom_line(ggplot2::aes(color = varname), show.legend = FALSE) +
    ggplot2::geom_hline(
      data = overall_means,
      ggplot2::aes(yintercept = overall_mean),
      linetype = "dashed",
      color = "black",
      alpha = 0.7
    ) +
    ggplot2::facet_wrap(~varname, scales = "free_y", ncol = 1) +
    ggplot2::labs(
      title = sprintf("Buoy Data Timeseries: %s", toupper(station_id)),
      subtitle = ifelse(is.null(agg_time), "Raw Data", sprintf("Aggregated by %s", agg_time)),
      x = "Time",
      y = "Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # 6. Save File
  if (write) {
    if (is.null(file_name) || trimws(file_name) == "") {
      stop("A valid file_name must be provided if write = TRUE.", call. = FALSE)
    }
    # Save using standard dimensions
    ggplot2::ggsave(filename = file_name, plot = p, width = 8, height = 6)
  }
  
  return(p)
}