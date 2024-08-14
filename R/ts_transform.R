#' Time series transformation
#' `r lifecycle::badge("experimental")`
#'
#' This function applies a transformation function to the values of a time series object and returns
#' a new time series object with the transformed values.
#'
#' @param .ts A time series object of class `ts` or `mts`.
#' @param .t A transformation function to be applied to the values of the time series.
#' @param .subset Optional character vector of column names to be transformed.
#' If NULL (default), all columns are transformed.
#'
#' @return A time series object with transformed values.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   log_passengers <- dts_transform(AirPassengers, log)
#'   log_passengers
#'
#'   # Transform only specific columns (if it were multivariate)
#'   # dts_transform(AirPassengers, sqrt, .subset = c("Column1", "Column2"))
#' }
dts_transform <- function(.ts, .t, .subset = NULL){
  if(is.null(.subset)){
    .ts |>
      dts_ts_to_df() |>
      dplyr::mutate(dplyr::across(-date, .t)) |>
      dts_df_to_ts()
  } else {
    .ts |>
      dts_ts_to_df() |>
      dplyr::mutate(dplyr::across(dplyr::all_of(.subset), .t)) |>
      dts_df_to_ts()
  }
}
