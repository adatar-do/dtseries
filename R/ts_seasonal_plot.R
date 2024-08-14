#' Create a seasonal plot for time series data
#'  `r lifecycle::badge("experimental")`
#'
#' This function generates a seasonal plot, visualizing time series data by year and season
#' (e.g., quarters or months).
#'
#' @param data A time series object of class `ts` or a data frame with a "date" column and numeric columns
#'   representing the time series.
#' @param .seasons An optional vector of seasons to include in the plot (e.g., 1:4 for quarters, 1:12
#'   for months). If NULL (default), all seasons are included.
#' @param .smooth Logical. Should a smooth trend line be added to the plot? Default is FALSE.
#' @param .freq The frequency of the time series (e.g., 12 for monthly, 4 for quarterly). If NULL (default),
#'   the frequency is automatically estimated from the data.
#' @param ... Additional arguments passed to `ggplot2::geom_smooth()`.
#'
#' @return A ggplot object displaying the seasonal plot.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Using a data frame
#'   data(economics, package = "ggplot2")
#'   seasonal_plot(economics, var_name = "unemploy", .freq = 12)
#'
#'   # Using a time series object
#'   data(AirPassengers)
#'   seasonal_plot(AirPassengers)
#'
#'   # Plot only specific quarters
#'   seasonal_plot(AirPassengers, .seasons = c(1, 4))
#' }
seasonal_plot <- function(data, .seasons = NULL, .smooth = FALSE, .freq = NULL, ...) {
  year <- NULL
  season <- NULL
  value <- NULL
  if ("ts" %in% class(data)) {
    data <- dts_ts_to_df(data)
  }
  if (is.null(.freq)) {
    .freq <- ts_guest_freq(data, "date")
  }
  data <- data |>
    dplyr::mutate(
      year = lubridate::year(date),
      season = dplyr::case_when(
        .freq == 4 ~ lubridate::quarter(date),
        .freq == 12 ~ lubridate::month(date)
        ),
      .keep = "unused"
    )

  if (!is.null(.seasons)) {
    data <- data |>
      dplyr::filter(season %in% .seasons)
  }

  data <- data |>
    dplyr::mutate(
      season = dplyr::case_when(
        .freq == 4 ~ paste0("Q", season),
        .freq == 12 ~ paste0("M", stringr::str_pad(season, 2, pad = "0"))
      ),
      .keep = "unused"
    )

  gplot <- data |>
    tidyr::pivot_longer(-c(year, season)) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = season)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~name, scales = "free_y", ncol = 2) +
    ggplot2::theme_bw()
  if (.smooth) {
    gplot <- gplot +
      ggplot2::geom_smooth(...)
  }
  gplot
}
