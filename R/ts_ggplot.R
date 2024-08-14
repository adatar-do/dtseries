#' Time series visualization with ggplot2
#' `r lifecycle::badge("experimental")`
#'
#' This function creates a ggplot object for time series data, providing options for faceting,
#' adding horizontal lines, and customizing the appearance.
#'
#' @param ts A time series object of class `ts` or `mts`.
#' @param .facet Logical. Should the plot be faceted by series? Default is TRUE.
#' @param .hline Numeric. Value for a horizontal line to be added to the plot. Default is NULL (no line).
#' @param .ncol Integer. Number of columns for facetting. Default is 1.
#' @param .select Character vector. Names of columns to select (if `ts` is multivariate). Default is NULL (all columns).
#'
#' @return A ggplot object representing the time series.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(nottem)
#'   dts_ts_ggplot(nottem, .facet = TRUE, .hline = 50)
#'
#'   # Univariate example
#'   data(AirPassengers)
#'   dts_ts_ggplot(AirPassengers, .facet = FALSE)
#' }
dts_ts_ggplot <- function(ts, .facet = TRUE, .hline = NULL, .ncol = 1, .select = NULL) {
  name <- NULL
  value <- NULL

  if (!is.null(.select)) {
    ts <- ts |>
      dts_select(.select)
  }

    ts |>
    dts_ts_to_df() |>
    tidyr::pivot_longer(-date) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = name)) +
    ggplot2::geom_line() -> gplot

  if (!is.null(.hline)) {
    gplot <- gplot +
      ggplot2::geom_hline(yintercept = .hline, color = "lightgray")
  }

  if (.facet && inherits(ts, "mts")) {  # Only facet if multivariate
    gplot <- gplot +
      ggplot2::facet_wrap(~name, scales = "free_y", ncol = .ncol)
  }

  gplot <- gplot +
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::xlab("")

  if (!inherits(ts, "mts")) {
    gplot <- gplot +
      ggplot2::theme(legend.position = "none")
  }

  gplot
}
