#' STL decomposition and plot
#'  `r lifecycle::badge("experimental")`
#'
#' This function performs STL (Seasonal-Trend Decomposition of time series) decomposition
#' on a time series and generates a plot of the decomposed components (trend, seasonal, and residual).
#'
#' @param data A data frame or a time series object.
#' @param var_name The name of the variable to decompose (required if `data` is a data frame).
#' @param date_var The name of the date/time variable in the data frame (default is "date").
#' @param .freq The frequency of the time series (required if `data` is a data frame).
#' @param .select The variables to select from the data frame (used only if `data` is a data frame).
#' @param .report Logical. Should a summary of the decomposition be printed? Default is TRUE.
#' @param ... Additional arguments passed to `stats::stl()`.
#'
#' @return A plot of the decomposed components and, if `.report = TRUE`, a list of STL objects.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(economics, package = "ggplot2")
#'   df <- economics
#'
#'   # Decompose and plot the unemployment rate
#'   dts_stl_plot(df, var_name = "unemploy", date_var = "date", .freq = 12)
#'
#'   # Example with a time series object
#'   data(AirPassengers)
#'   dts_stl_plot(AirPassengers, .freq = 12)
#' }
dts_stl_plot <- function(data, var_name = NULL, date_var = "date", .freq = NULL, .select = NULL, .report = TRUE, ...) {
  df <- data
  if ("ts" %in% class(df)) {
    if (!is.null(.select)) {
      df <- df |> dts_select(.select)
    }
    df <- df |> dts_ts_to_df()
  } else {
    if (!is.null(.select)) {
      df <- df |> dplyr::select(dplyr::all_of(.select))
    }
  }
  if (is.null(var_name)) {
    result <- list()
    for (.name in names(df)[names(df) != date_var]) {
      result[[.name]] <- stl_plot0(df, .name, date_var, .freq, ...)
    }
  } else {
    return(stl_plot0(df, var_name, date_var, .freq, ...))
  }
  if (.report) {
    for (.name in names(result)) {
      print(cli::rule(center = .name))
      print(result[[.name]])
    }
    invisible(result)
  } else {
    result
  }
}



stl_plot0 <- function(df, var_name, date_var = "date", .freq = NULL, ...) {
  . <- NULL
  name <- NULL
  value <- NULL
  df |>
    dplyr::select(
      {{ date_var }},
      {{ var_name }}
    ) |>
    dts_df_to_ts(.date = date_var) -> ts_df

  decomposed_ts <- stats::stl(ts_df, s.window = "periodic", ...)

  # trend <- decomposed_ts$time.series[, 1]
  # seasonal <- decomposed_ts$time.series[, 2]
  # residual <- decomposed_ts$time.series[, 3]

  df <- df |>
    dplyr::select({{ date_var }}, {{ var_name }})

  decomposed_ts$time.series[, 1] |>
    xts::as.xts() %>%
    as.data.frame() %>%
    .[["V1"]] -> df[["seasonal"]]

  decomposed_ts$time.series[, 2] |>
    xts::as.xts() %>%
    as.data.frame() %>%
    .[["V1"]] -> df[["trend"]]

  decomposed_ts$time.series[, 3] |>
    xts::as.xts() %>%
    as.data.frame() %>%
    .[["V1"]] -> df[["residual"]]

  df[["date"]] <- df[[date_var]]

  df |>
    tidyr::pivot_longer(-{{ date_var }}) |>
    dplyr::mutate(name = factor(name, levels = c({{ var_name }}, "trend", "seasonal", "residual"))) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~name, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::xlab("")
}
