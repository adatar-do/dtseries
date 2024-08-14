#' Set a name to a time series
#' `r lifecycle::badge("experimental")`
#'
#' This function sets a custom name to a time series object, storing the name in
#' the `.name` attribute.
#'
#' @param .ts A time series object of class `ts` or `mts`.
#' @param .name A character string specifying the name to assign to the time series.
#'
#' @return The time series object with the name set in the `.name` attribute.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   ts_air <- dts_name(AirPassengers, "AirPassengersData")
#'   attr(ts_air, ".name")
#' }
dts_name <- function(.ts, .name) {
    attr(.ts, ".name") <- .name
    .ts
}
