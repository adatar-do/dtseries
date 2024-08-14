#' Detect and visualize breakpoints in time series data
#'  `r lifecycle::badge("experimental")`
#'
#' This function uses the `strucchange` package to detect breakpoints in time series data
#' and generates plots to visualize the detected breakpoints.
#'
#' @param .ts A time series object of class `ts`.
#' @param .n The maximum number of breakpoints to consider. If NULL (default), a maximum of 3 breakpoints
#'   or a third of the number of years in the time series (whichever is smaller) is used.
#' @param .bps The type of breakpoint information to return:
#'   - "s": A summary of the breakpoint analysis (default).
#'   - "a": The raw breakpoint object from `strucchange::breakpoints()`.
#'   - "n": No breakpoint information, only plots are returned.
#' @param ... Additional arguments passed to `strucchange::breakpoints()`.
#'
#' @return A list containing:
#'   - Plots visualizing the detected breakpoints for each considered number of breaks.
#'   - Optionally, breakpoint information (`summary` or the `breakpoints` object) based on the `.bps` argument.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   bp_analysis <- dts_breakpoints(AirPassengers, .n = 4)
#'   bp_analysis$bps
#'   bp_analysis[[1]]
#'
#'   # Return only plots
#'   bp_plots <- dts_breakpoints(AirPassengers, .n = 2, .bps = "n")
#' }
dts_breakpoints <- function(.ts, .n = NULL, .bps = c("s", "a", "n"), ...) {
  # Validate input time series
  if (!inherits(.ts, "ts")) {
    stop("Input '.ts' must be a time series object of class 'ts'.")
  }

  # Determine the maximum number of breakpoints
  if (is.null(.n)) {
    .n <- min(3, ceiling(length(.ts) / (stats::frequency(.ts) * 3)))
  }

  # Handle .bps argument
  .bps <- match.arg(.bps)

  # Convert to data frame
  ts_data <- dts_ts_to_df(.ts)

  # Generate plots and breakpoint analysis
  res <- purrr::map(1:.n, function(.x) {
    .x <<- .x
    bps <- strucchange::breakpoints(ts_data[[names(ts_data)[names(ts_data) != 'date']]] ~ 1, breaks = .x, data = ts_data, ...)
    .gplot <- dts_ts_ggplot(.ts)
    for (.bp in bps$breakpoints) {
      .gplot <- .gplot +
        ggplot2::geom_vline(xintercept = ts_data[["date"]][[.bp]], linetype = "dashed")
    }
    .gplot
  })

  # Add breakpoint information to the list
  if (.bps == "s") {
    res[["bps"]] <- summary(strucchange::breakpoints(ts_data[[names(ts_data)[names(ts_data) != 'date']]] ~ 1, breaks = .n, data = ts_data, ...))
  } else if (.bps == "a") {
    res[["bps"]] <- strucchange::breakpoints(ts_data[[names(ts_data)[names(ts_data) != 'date']]] ~ 1, breaks = .n, data = ts_data, ...)
  }

  res
}
