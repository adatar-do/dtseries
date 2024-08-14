#' Merge multiple time series
#' `r lifecycle::badge("experimental")`
#'
#' This function merges multiple time series objects based on their dates. It handles
#' both univariate (`ts`) and multivariate (`mts`) time series. If the input time
#' series are not of class `mts`, the column names will be used to label the
#' respective series in the merged data.
#'
#' @param ... Time series objects of class `ts` or `mts`.
#' @param .by The column to use for merging. Defaults to "date".
#' @param .type The type of join to perform. Can be one of "full", "inner", "left",
#'   or "right". See `dplyr::full_join` for more information. Defaults to "full".
#'
#' @return A merged time series object of class `mts`.
#' @export
#'
#' @examples
#' \dontrun{
#'   ts1 <- ts(rnorm(100), start = c(2010, 1), frequency = 12)
#'   ts2 <- ts(rnorm(50), start = c(2010, 1), frequency = 12)
#'   ts3 <- ts(rnorm(120), start = c(2010, 1), frequency = 12)
#'   merged_ts <- dts_merge(ts1, ts2, ts3)
#'   merged_ts
#' }
dts_merge <- function(..., .by = 'date', .type = 'full'){
  data <- list(...)
  res <- list()
  if(is.null(names(data))){
    names(data) <- paste0("ts", seq_along(data))
  }
  for (.num in seq_along(data)) {
    .ts <- names(data)[.num]
    .d <- data[[.num]] %>% dts_name(ifelse(.ts == "", paste0("ts", .num), .ts))
    .subres <- .d |>
      dts_ts_to_df()
    if(!"mts" %in% class(.d)){
      names(.subres)[names(.subres) == 'ts'] <- .ts
    }
    if(length(res) == 0){
      res <- .subres
    } else {
      res <- utils::getFromNamespace(paste0(.type, '_join'), 'dplyr')(res, .subres, .by)
    }
  }
  res |>
    dts_df_to_ts()
}
