#' Select columns from a multivariate time series
#'
#' This function allows you to select specific columns from a multivariate time series (`mts`) object
#' by specifying column names to include or exclude, while preserving the time series structure.
#'
#' @param data A multivariate time series object of class `mts`.
#' @param .include A character vector of column names to include.
#' @param .exclude A character vector of column names to exclude.
#'
#' @return A multivariate time series object (`mts`) containing only the selected columns.
#' @export
#'
#' @examples
#'\dontrun{
#'   data(ausair)
#'   dts_select(ausair, .include = c("Q2", "Q3", "Q4"))
#'   dts_select(ausair, .exclude = "Q1")
#' }
dts_select <- function(data, .include = NULL, .exclude = NULL){

  # Check if data is a multivariate time series
  if (!inherits(data, "mts")) {
    stop("Input 'data' must be a multivariate time series object of class 'mts'.")
  }

  # Handle cases where both .include and .exclude are NULL
  if (is.null(.include) && is.null(.exclude)) {
    return(data)
  }

  # Convert to data frame
  data_df <- dts_ts_to_df(data)

  # Apply selection based on .include
  if (!is.null(.include)) {
    .include <- .include[.include != "date"]
    if (!all(.include %in% names(data_df))) {
      stop("Some included columns are not present in the time series.")
    }
    data_df <- data_df |>
      dplyr::select(date, dplyr::all_of(.include))
  }

  # Apply selection based on .exclude
  if (!is.null(.exclude)) {
    .exclude <- .exclude[.exclude != "date"]
    if (!all(.exclude %in% names(data_df))) {
      stop("Some excluded columns are not present in the time series.")
    }
    data_df <- data_df |>
      dplyr::select(-dplyr::all_of(.exclude))
  }

  # Convert back to time series
  dts_df_to_ts(data_df)
}
