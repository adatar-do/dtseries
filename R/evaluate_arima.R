#' Evaluate an ARIMA model performance
#'  `r lifecycle::badge("experimental")`
#'
#' Evaluate ARIMA model performance, including training metrics, residual checks, and forecasting performance.
#'
#' @param model An ARIMA model object returned by arima().
#' @param valid A time series object with the validation set, if applicable. Default is NULL.
#' @param test A time series object with the test set, if applicable. Default is NULL.
#' @param xreg An optional regression variable for the model.
#' @param .train_stats A boolean to indicate whether to include training metrics (TRUE) or not (FALSE). Default is TRUE.
#' @param .resid_tests A boolean to indicate whether to include residual checks (TRUE) or not (FALSE). Default is TRUE.
#' @param .transf A function or a list of functions to be applied to the observed and fitted time series. This is useful for handling models that include transformations.
#' @param .metrics A vector of metrics to be calculated. Default is c("rmse", "mae", "mape", "smape").
#' @param .report A boolean to indicate whether to print the report (TRUE) or not (FALSE). Default is TRUE.
#' @param ... Not implemented.
#'
#' @return A list with the following elements:
#' \item{model-summary}{A summary of the model}
#' \item{training_metrics}{A matrix of training metrics (if .train_stats is TRUE)}
#' \item{residual_checks}{A list of results of the residual checks (if .resid_tests is TRUE)}
#' \item{forecast-metrics}{A matrix of forecast metrics (if validation and test sets are supplied)}
#' \item{pred}{A data frame with the predictions and observed values}
#' \item{plot_pred}{A ggplot object with a plot of the observed and predicted values}
#' \item{plot_rmse}{A ggplot object with a plot of the RMSE values}
#' \item{plot_mae}{A ggplot object with a plot of the MAE values}
#' \item{plot_mape}{A ggplot object with a plot of the MAPE values}
#' \item{plot_smape}{A ggplot object with a plot of the sMAPE values}
#' \item{notes}{A vector of notes about the evaluation process}
#'
#' @examples
#' \dontrun{
#'   model <- arima(lynx, order = c(1, 1, 1))
#'   evaluate_arima(model)
#' }
#'
evaluate_arima <- function(
    model,
    valid = NULL,
    test = NULL,
    xreg = NULL,
    .train_stats = TRUE,
    .resid_tests = TRUE,
    .transf = NULL,
    .metrics = c("rmse", "mae", "mape", "smape"),
    .report = TRUE,
    ...) {
      ts <- NULL
      outlier <- NULL
      .lab <- NULL
      rownumberrr <- NULL
      el_grupos <- NULL
  args <- list(...)
  .interactive <- args[[".interactive"]]
  result <- list()
  metric <- c()
  observed <- model$x
  fitted <- model$fitted

  if (!is.null(.transf)) {
    for (.t in .transf) {
      observed <- .t(observed)
      fitted <- .t(fitted)
      if (!is.null(valid)) {
        valid <- .t(valid)
      }
      if (!is.null(test)) {
        test <- .t(test)
      }
    }
  }

  note1 <- NULL

  if (.train_stats) {
    result[["model-summary"]] <- summary(model)

    rlang::check_installed("yardstick")
    for (m in .metrics) {
      metric_func <- utils::getFromNamespace(paste0(m, "_vec"), "yardstick")
      if (!is.function(metric_func)) {
        stop(paste0("The function `", m, "` is not available in the `yardstick` package"))
      }
      metric[toupper(m)] <- round(metric_func(c(observed), c(fitted)), 4)
    }
    note1 <- "Note 1: The training metrics are different from those in the model summary
    because the indicated transformations were applied to the former,
    while not to the summary ones."
    note2 <- NULL
    result[["training_metrics"]] <- as.matrix(metric)
    if (.resid_tests) {
      rlang::check_installed("patchwork")
      if (!"package:patchwork" %in% search()) {
        requireNamespace('patchwork')
      }
      result[["residual_checks"]] <- list(
        "non-autocorrelation" = stats::Box.test(model$residuals, min(10, nrow(model$residuals) / 5), type = "Ljung-Box"),
        "non-heteroscedasticity" = lmtest::bptest(model$residuals ~ model$fitted),
        "normality" = stats::ks.test(model$residuals, "pnorm", mean = 0, sd = 1),
        "unit-root" = tseries::adf.test(model$residuals)
      )
      # Create the line plot
      .line <- model$residuals %>%
        dts_ts_to_df() %>%
        dplyr::mutate(
          ts = as.numeric(ts),
          outlier = ifelse(ts %in% graphics::boxplot(model$residuals, plot = FALSE)$out, "Outlier", "Not Outlier"),
          .lab = dplyr::if_else(outlier == "Outlier", as.character(date), "")
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = ts, group = 1)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(ggplot2::aes(color = outlier)) +
        ggplot2::geom_text(ggplot2::aes(label = .lab), size = 2) +
        ggplot2::geom_hline(yintercept = 0, color = "lightgray") +
        ggplot2::scale_color_manual(values = c("Not Outlier" = "lightblue", "Outlier" = "red")) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_color_discrete(guide = ggplot2::guide_legend(NULL)) +
        ggplot2::xlab("") +
        ggplot2::ylab("")

      # Create the ACF plot
      .acf <- model$residuals %>%
        forecast::Acf(plot = FALSE) %>%
        ggplot2::autoplot() +
        ggplot2::theme_bw()

      # Create the density plot
      .hist <- model$residuals %>%
        dts_ts_to_df() %>%
        utils::type.convert(as.is = TRUE) |>
        ggplot2::ggplot(ggplot2::aes(x = ts)) +
        ggplot2::geom_density() +
        ggplot2::theme_bw() +
        ggplot2::xlab("") +
        ggplot2::ylab("")

      # Combine the plots
      result[["residual_plot"]] <- (.line / (.acf + .hist))
    }
  }

  if (!is.null(valid)) {
    type <- c(
      rep("train", length(c(observed))),
      rep("valid", length(c(valid)))
    )
    if (!is.null(test)) {
      type <- c(type, rep("test", length(c(test))))
    }
    date <- dts_concat(stats::time(observed), stats::time(valid))
    if (!is.null(test)) {
      date <- dts_concat(date, stats::time(test))
    }
    date <- dts_ts_to_df(date)[["date"]]
    truth <- c(
      observed,
      valid
    )
    if (!is.null(test)) {
      truth <- c(truth, test)
    }
    p <- length(c(valid))
    if (!is.null(test)) {
      p <- p + length(c(test))
    }
    fcs <- forecast::forecast(
      model,
      p,
      xreg = xreg
    )["mean"][[1]]

    if (!is.null(.transf)) {
      for (.t in .transf) {
        fcs <- .t(fcs)
      }
    }

    estimate <- c(
      fitted,
      fcs
    )

    metric <- c()
    rlang::check_installed("yardstick")
    for (m in .metrics) {
      metric_func <- utils::getFromNamespace(paste0(m, "_vec"), "yardstick")
      if (!is.function(metric_func)) {
        stop(paste0("The function `", m, "` is not available in the `yardstick` package"))
      }
      metric[toupper(m)] <- round(metric_func(utils::tail(c(truth), sum(type != "train")), utils::tail(c(estimate), sum(type != "train"))), 4)
    }
    result[["forecast-metrics"]] <- as.matrix(metric)

    res <- data.frame(
      type,
      date,
      truth,
      estimate
    )
    res <- res |>
      dplyr::mutate(rownumberrr = dplyr::row_number()) |>
      split(~rownumberrr)
    if (
      any(
        all("mae" %in% .metrics, "rmse" %in% .metrics)
      )
    ) {
      note2 <- "Note 2: Some calculations (table and graphs) are done at the row level,
      so some metrics like 'RMSE' and 'MAE' will give the same results."
    }
    for (.m in .metrics) {
      # if (.m %in% c("rmse")) {
      #  .metrics <- .metrics[.metrics != .m]
      # } else {
      res <- res |>
        purrr::map(calc_metric, .m)
      # }
    }

    res <- dplyr::bind_rows(res) |>
      dplyr::select(-rownumberrr)

    res <- res |>
      dplyr::group_by(el_grupos = type == "train") |>
      dplyr::mutate(dplyr::across(dplyr::all_of(.metrics), dplyr::cummean, .names = "{.col}_cum")) |>
      dplyr::ungroup() |>
      dplyr::select(-el_grupos)

    result[["pred"]] <- utils::tail(
      res,
      ifelse(
        !is.null(test),
        length(valid) + length(test),
        length(valid)
      )
    ) |>
      dplyr::mutate(dplyr::across(is.numeric, round, 4)) |>
      as.data.frame()

    if (all(!is.null(.interactive), .interactive)) {
      result[["pred"]] <- DT::datatable(result[["pred"]])
    }

    res |>
      ggplot2::ggplot(ggplot2::aes(x = date)) +
      ggplot2::geom_line(ggplot2::aes(y = truth)) +
      ggplot2::geom_line(ggplot2::aes(y = estimate, color = type)) +
      ggplot2::theme_bw() -> result[["plot_pred"]]

    for (m in .metrics) {
      res %>%
        ggplot2::ggplot(ggplot2::aes(x = date)) +
        ggplot2::geom_col(ggplot2::aes(y = !!as.name(m), fill = type)) +
        ggplot2::geom_line(ggplot2::aes(y = !!as.name(paste0(m, "_cum")))) +
        ggplot2::theme_bw() -> result[[paste0("plot_", m)]]
    }
  }
  notes <- c(note1, note2)
  if (length(notes) > 0) {
    result[["notes"]] <- notes
  }
  if (.report) {
    .reporter(result)
    invisible(result)
  } else {
    result
  }
}


calc_metric <- function(x, .m) {
  metric_func <- utils::getFromNamespace(paste0(.m, "_vec"), "yardstick")
  x[[.m]] <- metric_func(x$truth, x$estimate)
  x
}

.reporter <- function(x, .rule = "center") {
  for (.name in names(x)) {
    if (.rule == "center") {
      print(cli::rule(center = .name))
    } else if (.rule == "left") {
      print(cli::rule(left = .name))
    }
    if ("list" %in% class(x[[.name]]) & length(class(x[[.name]])) == 1) {
      .reporter(x[[.name]], .rule = "left")
    } else {
      print(x[[.name]])
    }
  }
}
