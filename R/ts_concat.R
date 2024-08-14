#' Concatenate time series objects
#'  `r lifecycle::badge("experimental")`
#'
#' This function concatenates multiple time series objects into a single time series object,
#' ensuring that the concatenated series is contiguous.
#'
#' @param ... Time series objects (`ts` or `mts`) to be concatenated.
#'
#' @return A concatenated time series object.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   ts1 <- window(AirPassengers, end = c(1955, 12))
#'   ts2 <- window(AirPassengers, start = c(1956, 1))
#'   concatenated_ts <- dts_concat(ts1, ts2)
#'   concatenated_ts
#' }
dts_concat <- function(...) {
  ts_list <- list(...)

  # Check for contiguous time series
  if (length(ts_list) > 1) {
    for (i in 1:(length(ts_list) - 1)) {
      .check_continuity(ts_list[[i]], ts_list[[i + 1]])
    }
  }

  # Concatenate time series data frames
  concatenated_df <- purrr::map(ts_list, dts_ts_to_df) |>
    dplyr::bind_rows()

  # Convert back to time series
  dts_df_to_ts(concatenated_df)
}

# Helper function to check for continuity between two time series
.check_continuity <- function(ts1, ts2) {
  last_date <- dts_ts_to_df(ts1) |>
    dplyr::pull(date) |>
    utils::tail(1)

  first_date <- dts_ts_to_df(ts2) |>
    dplyr::pull(date) |>
    utils::head(1)

  valid_diff <- dts_ts_to_df(ts1) |>
    dplyr::pull(date) |>
    diff() |>
    unique()

  if (!(as.numeric(difftime(first_date, last_date, units = "days")) %in% valid_diff)) {
    stop("Supplied time series are not contiguous.", call. = FALSE)
  }
}
