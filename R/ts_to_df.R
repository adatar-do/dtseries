#' Convert a time series object to a data.frame (Monthly/Quarterly)
#'  `r lifecycle::badge("experimental")`
#'
#' This function converts a time series object (`ts`) into a data frame, optionally
#' including a date column. It currently supports **monthly and quarterly** time series.
#'
#' @param ts A time series object of class `ts` or `mts` with a monthly or quarterly frequency.
#' @param as_date Logical. Should a date column be included in the output? Default is TRUE.
#' @param .name A character string specifying the name of the value column.
#' If NULL (default), the name will be taken from the `ts` object if available.
#' @param ... Additional arguments passed to other functions (currently unused).
#'
#' @return A data frame with a `date` column (if `as_date = TRUE`) and a
#' column containing the time series values.
#' @export
#'

# @examples
# \dontrun{
#   data(AirPassengers)
#   dts_ts_to_df(AirPassengers)
#   dts_ts_to_df(AirPassengers, .name = "air_passengers")
# }
dts_ts_to_df <- function(ts, as_date = TRUE, .name = NULL, ...) {
  year <- NULL
  month <- NULL
  x <- NULL


  # Check for supported frequencies
  supported_frequencies <- c(4, 12) # Monthly and quarterly
  if (!(stats::frequency(ts) %in% supported_frequencies)) {
    stop("Unsupported frequency. Currently, only monthly and quarterly time series are supported.")
  }

  months <- .ts_months(stats::frequency(ts))

  res <- ts |>
    stats::time() |>
    as.data.frame() |>
    stats::setNames(c("x")) |>
    dplyr::mutate(x = format(x, nsmall = 4)) |>
    tidyr::separate(col = x, into = c("year", "month"), sep = "\\.") |>
    dplyr::mutate(year = as.numeric(year), month = months[month]) |>
    cbind(ts %>% as.data.frame())

  if (as_date) {
    res <- res |>
      dts_vars_to_date(year = "year", month = "month")
  }

  if (!inherits(ts, "mts")) {
    if (!is.null(.name)) {
      value_col_name <- .name
    } else if (!is.null(attr(ts, ".name"))) {
      value_col_name <- attr(ts, ".name")
    } else {
      value_col_name <- deparse(substitute(ts))
    }
    if (as_date) {
      res <- stats::setNames(res, c("date", value_col_name))
    } else {
      res <- stats::setNames(res, c("year", "month", value_col_name))
    }

    attr(res, ".name") <- .name
  }


  res %>%
    dplyr::mutate(dplyr::across(-date, as.numeric))
}

.ts_months <- function(frequency) {
  months <- c(
    "0000" = 1,
    "0833" = 2,
    "1667" = 3,
    "2500" = 4,
    "3333" = 5,
    "4167" = 6,
    "5000" = 7,
    "5833" = 8,
    "6667" = 9,
    "7500" = 10,
    "8333" = 11,
    "9167" = 12
  )

  if (frequency == 4) {
    months <- months + 2
  }
  months
}
