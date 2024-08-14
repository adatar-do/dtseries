#' Create seasonal dummy variables for a time series
#'  `r lifecycle::badge("experimental")`
#'
#' This function generates seasonal dummy variables for a time series object, representing
#' quarters, months, or days, depending on the frequency of the time series.
#'
#' @param .ts A time series object of class `ts` or `mts`.
#' @param .periods An optional vector of periods (quarters, months, or days) to create dummy variables for.
#'   If NULL (default), dummy variables are created for all periods except the last one.
#' @param .rm Logical. Should existing data columns be removed from the output? Default is FALSE.
#'
#' @return A time series object with added seasonal dummy variables.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   ts_with_dummies <- dts_seas(AirPassengers)
#'   ts_with_dummies
#'
#'   # Create dummies for specific months
#'   ts_with_dummies <- dts_seas(AirPassengers, .periods = c(1, 7, 12))
#'   ts_with_dummies
#' }
dts_seas <- function(.ts, .periods = NULL, .rm = FALSE) {
  .freq <- .ts |>
    dts_ts_to_df() |>
    ts_guest_freq("date")
  .fn <- ifelse(
    .freq == 4,
    lubridate::quarter,
    ifelse(
      .freq == 12,
      lubridate::month,
      lubridate::day
    )
  )
  .prefix <- ifelse(
    .freq == 4,
    "Q",
    ifelse(
      .freq == 12,
      "M",
      "D"
    )
  )
  res <- .ts |>
    dts_ts_to_df()
  if (.rm) {
    res <- res |>
      dplyr::select(date)
  }
  if (is.null(.periods)) {
    .periods <- 1:(.freq - 1)
  }
  for (.p in .periods) {
    res <- res |>
      dplyr::mutate(
        !!paste0(.prefix, .p) := as.integer(.fn(date) == .p)
      )
  }
  res |>
    dts_df_to_ts()
}
