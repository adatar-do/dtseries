#' Subtract two time series
#' `r lifecycle::badge("experimental")`
#'
#' This function subtracts one time series (`ts2`) from another (`ts1`).
#'
#' @param ts1 The time series to subtract from (minuend).
#' @param ts2 The time series to subtract (subtrahend).
#'
#' @return A time series object representing the difference between `ts1` and `ts2`.
#' @export
#'
#' @examples
#' \dontrun{
#'   ts1 <- ts(rnorm(100), start = c(2010, 1), frequency = 12)
#'   ts2 <- ts(rnorm(50), start = c(2010, 1), frequency = 12)
#'   diff_ts <- dts_sub(ts1, ts2)
#'   diff_ts
#' }
dts_sub <- function(ts1, ts2){
  stats::window(
    ts1,
    start = stats::end(ts2) + c(0, 1)
  )
}
