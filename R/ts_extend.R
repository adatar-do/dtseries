#' Extend a time series to a specified end date
#'  `r lifecycle::badge("experimental")`
#'
#' This function extends a time series to a given end date, filling missing values with a default value.
#' It returns only the extended portion of the time series, excluding the original data.
#'
#' @param .ts The time series to be extended.
#' @param .end The desired end date for the extended time series.
#' @param .default Numeric. The default value to use for filling missing values in the extended portion. Default is 0.
#' @param .sub Logical. Should the original time series be subtracted from the extended time series,
#'   leaving only the extended part? Default is TRUE.
#'
#' @return A time series object containing only the extended portion of the original time series.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   dts_extend(AirPassengers, .end = c(1965, 12))
#'   dts_extend(AirPassengers, .end = c(1965, 12), .default = 100)
#'   dts_extend(AirPassengers, .end = c(1965, 12), .sub = FALSE)
#' }
dts_extend <- function(.ts, .end, .default = 0, .sub = TRUE) {
  # Extend the time series
  extended_ts <- stats::window(
    .ts,
    end = .end,
    frequency = stats::frequency(.ts),
    extend = TRUE
  )

  # Fill missing values
  extended_ts <- extended_ts |>
    dts_ts_to_df() |>
    dplyr::mutate(dplyr::across(-date, ~ tidyr::replace_na(.x, .default))) |>
    dts_df_to_ts()

  # Subtract original time series if requested
  if (.sub) {
    extended_ts <- dts_sub(extended_ts, .ts)  # Assuming dts_sub is defined
  }

  extended_ts
}
