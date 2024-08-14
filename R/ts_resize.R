#' Resize a time series to match another
#' `r lifecycle::badge("experimental")`
#'
#' This function resizes a time series to match the start and end dates of another time series.
#' It handles missing values by filling them with a specified default value.
#'
#' @param .ts The time series to be resized.
#' @param .resize The reference time series for resizing.
#' @param .start Logical. Should the start date of the resized time series match the start date of the
#'   reference time series? If FALSE, the original start date is kept. Default is TRUE.
#' @param .default Numeric. The default value to use for filling missing values. Default is 0.
#'
#' @return A resized time series object with the same frequency as the reference time series.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'
#'   # Resize to a shorter period
#'   short_ts <- window(AirPassengers, start = c(1955, 1), end = c(1958, 12))
#'   resized_ts <- dts_resize(AirPassengers, short_ts)
#'   resized_ts
#'
#'   # Resize to a longer period, keeping the original start date
#'   long_ts <- window(AirPassengers, start = c(1949, 1), end = c(1962, 12))
#'   resized_ts <- dts_resize(AirPassengers, long_ts, .start = FALSE, .default = 0)
#'   resized_ts
#' }
dts_resize <- function(.ts, .resize, .start = TRUE, .default = 0) {
  if (stats::frequency(.ts) != stats::frequency(.resize)) {
    stop("Frequency of input time series and resize time series do not match.")
  }
  res <- stats::window(
    .ts,
    start = ifelse(.start, stats::time(.resize), stats::time(.ts)),
    end = stats::end(.resize),
    frequency = stats::frequency(.resize),
    extend = TRUE
  )
  res |>
    dts_ts_to_df() |>
    dplyr::mutate(
      dplyr::across(
        -date,
        ~ tidyr::replace_na(.x, .default)
      )
    ) |>
    dts_df_to_ts()
}
