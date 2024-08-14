#' Calculate time changes (Interannual, Previous period, or Year-to-date cumulative)
#'  `r lifecycle::badge("experimental")`
#'
#' This function calculates time changes for a time series object, offering options for
#' interannual change, change from the previous period, or year-to-date cumulative change.
#'
#' @param .ts A time series object of class `ts` or `mts`.
#' @param .vars Optional character vector of column names to calculate time changes for. If NULL (default),
#'   all columns are used.
#' @param .type The type of time change to calculate:
#'   - "i": Interannual change (default)
#'   - "p": Change from the previous period
#'   - "c": Year-to-date cumulative change
#' @param .base The base for calculating the index or percentage change.
#'  ie. 100 for percentage change or 1 for index change. Default is 1.
#' @param drop_vars Logical. If TRUE (default), the original variables are dropped from the output.
#' @param na.omit Logical. If TRUE (default), remove rows with NA values.
#'
#' @return A time series object with the calculated time changes.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   # Interannual change
#'   dts_tc(AirPassengers, .type = "i")
#'
#'   # Change from the previous period
#'   dts_tc(AirPassengers, .type = "p")
#'
#'   # Year-to-date cumulative change
#'   dts_tc(AirPassengers, .type = "c")
#' }
dts_tc <- function(
    .ts,
    .vars = NULL,
    .type = c("i", "p", "c"),
    .base = 1,
    drop_vars = TRUE,
    na.omit = drop_vars
  ) {
  tc <- NULL
  year <- NULL

  df <- dts_ts_to_df(.ts)

  .names <- names(df)
  .names <- ifelse(is.null(.vars), .names[.names != "date"], .vars)
  .type <- ifelse(length(.type) > 1, .type[[1]], .type)

  .lags <- dplyr::case_match(
    .type,
    "i" ~ frequency(.ts),
    .default = 1
  )

  .ts2 <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(.names),
        .fns = ~ (.x / dplyr::lag(.x, .lags) - 1),
        .names = "{.col}_tc"
      ),
      .keep = ifelse(drop_vars, "unused", "all")
    ) %>%
    tidyr::drop_na() %>%
    dts_df_to_ts()

  if (.type == "c") {
    .ts2 <- .ts2 %>%
      dts_ts_to_df() %>%
      dplyr::group_by(year = lubridate::year(date)) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::ends_with("_tc"),
          .fns = ~ cumsum(.x)
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-year) %>%
      dts_df_to_ts()
  }

  .ts2 %>%
    dts_ts_to_df() %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_tc"),
        .fns = ~ .x * .base
      )
    ) %>%
    dts_df_to_ts()
}
