#' Plot Autocorrelation and Partial Autocorrelation Functions (ACF/PACF)
#'  `r lifecycle::badge("experimental")`
#'
#' This function creates a ggplot2-based plot of the Autocorrelation Function (ACF) and, optionally,
#' the Partial Autocorrelation Function (PACF) for a time series.
#'
#' @param data A time series object of class `ts` or `mts`, or a data frame with a "date" column and
#'   one or more numeric columns representing the time series.
#' @param .partial Logical. Should the PACF be plotted as well? Default is TRUE.
#' @param .mts Logical. Is the input data a multivariate time series (`mts`)? Default is FALSE.
#' @param .interactions Logical. Should interactions between series be considered (not yet implemented)?
#'   Default is FALSE.
#' @param .select Character vector. Names of columns to select (if `data` is multivariate). Default is NULL (all columns).
#'
#' @return A ggplot object displaying the ACF and/or PACF.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   dts_acf_plot(AirPassengers)
#'
#'   data(economics, package = "ggplot2")
#'   dts_acf_plot(economics, .select = c("psavert", "uempmed"))
#' }
dts_acf_plot <- function(data, .partial = TRUE, .mts = FALSE, .interactions = FALSE, .select = NULL){
  nombre <- NULL
  type <- NULL
  x <- NULL
  y <- NULL
  res <- list()
  if(all(.interactions, !"mts" %in% class(data))){
    .interactions <- FALSE
    warning("No 'mts' provided. Ignoring '.interactions'.")
  }
  if(.interactions){
    res[["acf"]] <- stats::acf(data, plot = F)
    res[['pacf']] <- stats::pacf(data, plot = F)
    stop("'.interactions' not implemented yet!")
  } else {
    if(!is.null(.select)){
      data <- dts_select(data, .select)
    }

    if(all(!.mts, "mts" %in% class(data))){
      data <- data[,1]
    }
    data <- dts_ts_to_df(data)
    nombres <- names(data)[names(data) != "date"]
    for (.n in nombres) {
      data |>
        dplyr::select(date, {{.n}}) |>
        dts_df_to_ts() -> .data

      .data |>
        stats::acf(plot = F) -> .acf
      .acf <- data.frame(x = .acf$lag, y = .acf$acf)
      res[[.n]] <- list(acf = .acf)
      if(.partial){
        .data |>
          stats::pacf(plot = F) -> .pacf
        .pacf <- data.frame(x = .pacf$lag, y = .pacf$acf)
        res[[.n]][['pacf']] <- .pacf
      }
      res[[.n]] <- dplyr::bind_rows(res[[.n]], .id = "type")
    }
  }
  .rows <- NULL
  .cols <- NULL
  if(ncol(data) > 2){
    .rows <- ggplot2::vars(nombre)
  }
  if(.partial){
  .cols <- ggplot2::vars(type)
      }
  res |>
    dplyr::bind_rows(.id = "nombre")|>
    dplyr::group_by(type, nombre) |>
    #dplyr::slice(-1) |>
    dplyr::filter(x != 0) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_segment(ggplot2::aes(xend = x, y = 0, yend = y)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = 2/sqrt(nrow(data)), color = "blue", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -2/sqrt(nrow(data)), color = "blue", linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::facet_grid(rows = .rows, cols = .cols, scales = "free") +
    ggplot2::ylab("") +
    ggplot2::xlab("")
}
