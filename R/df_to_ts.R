#' Convert a Data Frame to a Time Series Object
#' `r lifecycle::badge("experimental")`
#'
#' This function converts a data frame with a date column into a time series object (`ts` or `mts`).
#'
#' @param data A data frame containing the data.
#' @param .freq The frequency of the time series (e.g., 12 for monthly, 4 for quarterly). If NULL (default),
#'   the frequency is automatically estimated based on the date column.
#' @param .date The name of the date column in the data frame (default is "date").
#' @param end The end date of the time series. If NULL (default), the maximum date in the data is used.
#'
#' @return A time series object of class `ts` or `mts`.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(fdeaths, package = "datasets")
#'   ts_data <- data.frame(date = as.Date(time(fdeaths)), y = fdeaths)
#'   ts_obj <- dts_df_to_ts(ts_data, .freq = 12)
#'   ts_obj
#' }
dts_df_to_ts <- function(data, .freq = NULL, .date = 'date', end = NULL){

  # Validate date column
  if (!inherits(data[[.date]], c("Date", "POSIXct"))) {
    stop(".date column is not in a valid date format.")
  }
  if(
    any(
      diff(
        data[[.date]]
        ) != difftime(
          utils::tail(data[[.date]], -1),
          utils::head(data[[.date]], -1),
          units = .freq
          )
      )
    ) {
    stop("Jumps in date column.")
  }
  if(is.null(.freq)){
    .freq <- ts_guest_freq(data, .date)
  }
  .start = min(data[[.date]])
  if(.freq == 1){
    .start = lubridate::year(.start)
  } else if(.freq == 4){
    quarter = as.numeric(format(.start, "%m"))/3
    .start = lubridate::year(.start) + (quarter - 1)/4
  } else if(.freq == 12) {
    .start = lubridate::year(.start) + (lubridate::month(.start)-1)/12
  } else {
    .start <- .start
  }
  if(is.null(end)) {
    end = max(data[[.date]])
    if(.freq == 1){
      end = lubridate::year(end)
    } else if(.freq == 4){
      quarter = as.numeric(format(end, "%m"))/3
      end = lubridate::year(end) + (quarter - 1)/4
    } else if(.freq == 12) {
      end = lubridate::year(end) + (lubridate::month(end)-1)/12
    } else {
      end <- end
    }
  }
  .names <- names(data)[names(data) != .date]

  if (length(.names) == 1) {
    .ts <- stats::ts(
      data[[.names]],
      start = .start, end = end, frequency = .freq
      )
  } else {
  .ts <- stats::ts(
    data[, .names],
    start = .start, end = end, frequency = .freq
    )
  }
  if(!inherits(.ts, "mts")){
    attr(.ts, '.name') <- names(data)[names(data) != .date]
  }
  .ts
}

ts_guest_freq <- function(data, .date){

  if(is.numeric(data[[.date]])){
    data[[.date]] <- paste0(data[[.date]], "-01-01")
  }

  # Guess the frequency
  dates = as.Date(data[[.date]])
  delta = diff(dates)
  mode_delta = as.numeric(names(sort(table(delta), decreasing = TRUE)))[1]
  if(mode_delta < 28){
    .freq = 365.25
  } else if(mode_delta <= 85) {
    .freq = 12
  } else if(mode_delta < 365) {
    .freq = 4
  }else {
    .freq = 1
  }
  .freq
}


