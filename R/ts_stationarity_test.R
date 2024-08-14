#' Test stationarity of time series using Augmented Dickey-Fuller Test
#' `r lifecycle::badge("experimental")`
#'
#' This function performs the Augmented Dickey-Fuller (ADF) test to check for stationarity in
#' a time series. It handles both univariate and multivariate time series.
#'
#' @param data A time series object of class `ts` or `mts`.
#' @param .select An optional character vector of column names to select for testing
#' (used only if `data` is a `mts` object).
#'
#' @return A list of ADF test results (for multivariate input) or a single ADF test result (for univariate input).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Univariate example
#'   data(AirPassengers)
#'   ts_stationarity_test(AirPassengers)
#'
#'   # Multivariate example
#'   ts1 <- ts(rnorm(100), start = c(2010, 1), frequency = 12)
#'   ts2 <- ts(rnorm(100), start = c(2010, 1), frequency = 12)
#'   data <- dts_merge(ts1, ts2)
#'   dts_stationarity_test(data)
#'   dts_stationarity_test(data, .select = "ts1")
#' }
dts_stationarity_test <- function(data, .select = NULL){
  if("mts" %in% class(data)){
    if(!is.null(.select)){
      data <- data |> dts_select(.select)
    }
    data |>
      dts_ts_to_df() |>
      dplyr::select(-date) |>
      purrr::map(tseries::adf.test)
  } else {
    tseries::adf.test(data)
  }
}
