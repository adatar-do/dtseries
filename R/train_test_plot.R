#' Visualize Train and Test Sets
#'
#' This function creates a plot to visualize the split of time series data into train and test sets
#' (and optionally a validation set). It can accept either the output of `dts_train_test_split()` or
#' multiple time series objects as individual arguments.
#'
#' @param ... Time series objects of class `ts` or data frames representing the train, test, and optionally
#'   validation sets. Accepts either a named list output from `dts_train_test_split()` or individual time
#'   series objects. The order of the arguments determines the plotting order and legend labels.
#' @param .select An optional character vector of column names to select for plotting. If NULL (default),
#'   all columns are plotted.
#'
#' @return A ggplot object displaying the train/test split.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'
#'   # Using output of dts_train_test_split()
#'   split_data <- dts_train_test_split(AirPassengers, prop = 0.2)
#'   train_test_plot(split_data)
#'
#'   # Using individual time series objects
#'   train_test_plot(dts_train_split(split_data), dts_test_split(split_data))
#' }
train_test_plot <- function(..., .select = NULL) {
  value <- NULL
  Set <- NULL
  data <- purrr::list_flatten(list(...))
  res <- list()
  .exclude <- c("Set")
  for (.name in names(data)) {
    if ("ts" %in% class(data[[.name]])) {
      .data <- data[[.name]]
      if (!is.null(.select)) {
        .data <- .data |> dts_select(.include = .select)
      }
      .data <- .data %>% dts_ts_to_df()
      .exclude <- append(.exclude, "date")
    } else {
      .data <- data[[.name]]
      if (!is.null(.select)) {
        .data <- dplyr::select(.data, .select)
      }
    }
    res[[.name]] <- .data
  }
  .exclude <- unique(.exclude)
  res |>
    dplyr::bind_rows(.id = "Set") |>
    tidyr::pivot_longer(-.exclude) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = Set)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~name, ncol = 1, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::theme(legend.position = "bottom")
}
