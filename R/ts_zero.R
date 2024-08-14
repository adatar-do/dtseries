#' Create a time series object filled with zeros
#' `r lifecycle::badge("experimental")`
#'
#' This function creates a time series object filled with zeros, with the same length and frequency
#' as the input time series. Optionally, you can resize the output time series using the `resize` argument.
#'
#' @param x A time series object of class `ts` or `mts`.
#' @param .name An optional name for the output time series. Default is NULL.
#' @param resize An optional time series object to use for resizing the output. Default is NULL (no resizing).
#'
#' @return A time series object filled with zeros, with the same class as the input.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   zero_ts <- dts_zero(AirPassengers)
#'   zero_ts
#'
#'   # Resize the output
#'   zero_ts_resized <- dts_zero(AirPassengers, resize = window(AirPassengers, start = c(1955, 1)))
#'   zero_ts_resized
#' }
dts_zero <- function(x, .name = NULL, resize = NULL) {
  if(!is.null(resize)){
    x <- dts_resize(x, resize)
  }
  x[] <- 0
  if(!is.null(.name)){
    attr(x, ".name") <- .name
  }
  x
}
