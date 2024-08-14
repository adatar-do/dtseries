#' Create time series dummy variables
#' `r lifecycle::badge("experimental")`
#'
#' This function generates time series dummy variables based on provided dates, a time series object,
#' or start/end/frequency specifications.
#'
#' @param dates A vector, data frame, or list of dates to set as 1 in the dummy variable.
#' @param start The start date of the time series (required if `ts` is not provided).
#' @param end The end date of the time series (required if `ts` is not provided).
#' @param frequency The frequency of the time series (required if `ts` is not provided).
#' @param .name An optional name for the output time series. Default is NULL.
#' @param ts A time series object (`ts` or `mts`). If provided, `start`, `end`, and `frequency` are ignored.
#' @param method The comparison operator to use when matching dates: "<", "<=", "==", ">=", ">". Default is "==".
#' @param ... Additional arguments to be passed to `ts`.
#'
#' @return A time series object (`ts` or `mts`) representing the dummy variable(s).
#' @export
#'
#' @examples
#'\dontrun{
#'   # Using dates and start/end/frequency
#'   tsd1 <- dts_dummy(dates = 2005, start = 2000, end = 2011, frequency = 1)
#'   tsd1
#'
#'   tsd2 <- dts_dummy(dates = c(2005, 3), start = 2000, end = c(2011, 4), frequency = 4)
#'   tsd2
#'
#'   tsd3 <- dts_dummy(dates = data.frame(c(2005, 2006), c(8, 11)),
#'                    start = 2000, end = c(2011, 12), frequency = 12)
#'   tsd3
#'
#'   tsd4 <- dts_dummy(dates = list("2005M07" = c(2005, 7), "2006M11" = c(2006, 11)),
#'                    start = 2000, end = 2011, frequency = 12)
#'
#'   tsd3
#'
#'   # Using a time series object
#'   data(AirPassengers)
#'   tsd5 <- dts_dummy(dates = c(1955, 6), ts = AirPassengers)
#'   tsd5
#' }
dts_dummy <- function(dates, start = NULL, end = NULL, frequency = NULL, .name = NULL, ts = NULL, method = "==", ...) {
  if (is.null(ts) & is.null(start) & is.null(end) & is.null(frequency)) {
    stop("start, end and frequency must be provided if ts is not provided")
  }
  if (!is.null(ts)) {
    start <- min(stats::time(ts))
    end <- max(stats::time(ts))
    frequency <- stats::frequency(ts)
  }
  res <- list()
  if (inherits(dates, 'list')) {
    for (i in names(dates)) {
      res[[i]] <- ts_dummy0(
        dates = dates[[i]],
        start = start,
        end = end,
        frequency = frequency,
        .method = method,
        ...
      )
    }
    res <- do.call(cbind, res)
  } else {
    res <- ts_dummy0(
      dates = dates,
      start = start,
      end = end,
      frequency = frequency,
      .method = method,
      ...
    )
    if(!is.null(.name)){
      attr(res, '.name') <- .name
    }
  }
  res
}


ts_dummy0 <- function(dates, start = NULL, end = NULL, frequency = NULL, .name = NULL, ts = NULL, .method, ...) {
  tsd <- stats::ts(0, start = start, end = end, frequency = frequency, ...)
  if (is.vector(dates)) {
    tsd <- set_ones(tsd, dates, frequency, .method)
  } else {
    for (num in 1:nrow(dates)) {
      tsd <- set_ones(tsd, dates[num, ], frequency, .method)
    }
  }
  tsd
}

set_ones <- function(tsd, date, freq, .method) {
  if (!.method %in% c("<", "<=", "==", ">", ">=")) {
    stop("Invalid comparison operator.")
  }
  months <- .ts_months(freq)
  if (freq == 1) {
    if (.method == "==") {
      tsd[abs(eval(parse(text = paste0("stats::time(tsd) - date")))) <= 0.0001] <- 1
    } else {
      tsd[eval(parse(text = paste0("stats::time(tsd)", .method, "date")))] <- 1
    }
  } else if (freq > 1) {
    if (.method == "==") {
      tsd[abs(eval(parse(text = paste0("stats::time(tsd) - sum((date - c(0, 1)) / c(1, freq))")))) <= 0.0001] <- 1
    } else {
      tsd[eval(parse(text = paste0("stats::time(tsd)", .method, "sum((date - c(0, 1)) / c(1, freq))")))] <- 1
    }
  }
  tsd
}
