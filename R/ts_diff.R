#' Calculate the difference of a time series
#' `r lifecycle::badge("experimental")`
#'
#' This function calculates the difference of a time series, preserving the date and
#' `.name` attribute (if present). It provides more control over the differencing process
#' compared to the base `diff()` function.
#'
#' @param .ts A time series object of class `ts` or `mts`.
#' @param .n An integer specifying the number of lags to use for calculating the difference (default is 1).
#' @param .subset A character vector specifying the columns to be differentiated (default is all columns).
#' @param .na.omit Logical. Should missing values be omitted after differencing? Default is TRUE.
#' @param ... Additional arguments passed to other functions (currently unused).
#'
#' @return A time series object of the same class as the input, with the differences calculated.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   diff_ts <- dts_diff(AirPassengers)
#'   diff_ts
#'
#'   # Difference with 2 lags
#'   diff_ts_2 <- dts_diff(AirPassengers, .n = 2)
#'   diff_ts_2
#' }
dts_diff <- function(.ts, .n = 1, .subset = NULL, .na.omit = TRUE, ...) {
  .name <- NULL
  if (!"mts" %in% class(.ts)) {
    .name <- attr(.ts, ".name")
  }
  res <- .ts |>
    dts_ts_to_df()
  if (is.null(.subset)) {
    res <- res |>
      dplyr::mutate(dplyr::across(-date, ~ .x - dplyr::lag(.x, .n)))
  } else {
    res <- res |>
      dplyr::mutate(dplyr::across(dplyr::all_of(.subset), ~ .x - dplyr::lag(.x, .n)))
  }
  if (.na.omit) {
    res <- tidyr::drop_na(res)
  }
  res <- res |>
    dts_df_to_ts()
  if (!is.null(.name)) {
    attr(res, ".name") <- .name
  }
  res
}
