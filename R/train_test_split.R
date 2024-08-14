#' Split time series data into train and test Sets
#' `r lifecycle::badge("experimental")`
#'
#' This function splits time series data into train and test sets based on a specified proportion
#' or number of observations. It also provides an option to create a validation set.
#'
#' @param data A time series object of class `ts` or `mts`.
#' @param prop The proportion or number of observations for the testing set:
#'   - If `prop < 1`, it's treated as a proportion of the total observations.
#'   - If `prop >= 1`, it's treated as the exact number of observations.
#' @param val_split Logical. Should a validation set be created from the test set? Default is FALSE.
#'
#' @return A list containing the train and test sets, and optionally a validation set:
#'   - `train`: The training set.
#'   - `test`: The test set.
#'   - `valid`: The validation set (if `val_split = TRUE`).
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   split_data <- dts_train_test_split(AirPassengers, prop = 0.2)
#'   dts_train_split(split_data)
#'   dts_test_split(split_data)
#'
#'   # With a validation set
#'   split_data <- dts_train_test_split(AirPassengers, prop = 0.3, val_split = TRUE)
#'   dts_train_split(split_data)
#'   dts_valid_split(split_data)
#'   dts_test_split(split_data)
#' }
dts_train_test_split <- function(data, prop = 3 / 4, val_split = FALSE) {
  res <- list()
  if (prop < 1) {
    ntest <- ceiling(length(stats::time(data)) * prop)
  } else {
    ntest <- prop
  }
  end <- max(stats::time(data)[1:(length(stats::time(data)) - ntest)])
  start <- max(stats::time(data)[1:(length(stats::time(data)) - ntest + 1)])
  if ("mts" %in% class(data)) {
    res[["train"]] <- stats::window(data, end = end) |> dts_name(attr(data, ".name"))
    res[["test"]] <- stats::window(data, start = start)
  } else {
    res[["train"]] <- stats::window(data, end = end) |> dts_name(attr(data, ".name"))
    res[["test"]] <- stats::window(data, start = start)
  }
  if (val_split) {
    nvalid <- floor(ntest / 2)
    end <- max(stats::time(res[["test"]])[1:(length(stats::time(res[["test"]])) - nvalid)])
    start <- max(
      stats::time(res[["test"]])[1:(length(stats::time(res[["test"]])) - nvalid + 1)]
    )
    if ("mts" %in% class(data)) {
      res[["valid"]] <- stats::window(
        res[["test"]],
        end = end
      ) |> dts_name(attr(data, ".name"))
      res[["test"]] <- stats::window(
        data,
        start = start
      ) |> dts_name(attr(data, ".name"))
    } else {
      res[["valid"]] <- stats::window(
        res[["test"]],
        end = end
      ) |> dts_name(attr(data, ".name"))
      res[["test"]] <- stats::window(
        data,
        start = start
      ) |> dts_name(attr(data, ".name"))
    }
  }
  res
}

#' Get train set from a dataset split
#' `r lifecycle::badge("experimental")`
#'
#' @param tts A list containing the train, test, and optionally valid sets (output of `train_test_split`).
#'
#' @return The train set.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   split_data <- dts_train_test_split(AirPassengers, prop = 0.8)
#'   train_set <- dts_train_split(split_data)
#'   train_set
#' }
dts_train_split <- function(tts) {
  tts[["train"]]
}

#' Get validation set from a dataset split
#' `r lifecycle::badge("experimental")`
#'
#' @param tts A list containing the train, test, and optionally valid sets (output of `train_test_split`).
#'
#' @return The validation set (NULL if not present).
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   split_data <- dts_train_test_split(AirPassengers, prop = 0.8, val_split = TRUE)
#'   valid_set <- dts_valid_split(split_data)
#'   valid_set
#' }
dts_valid_split <- function(tts) {
  tts[["valid"]]
}

#' Get test set from a sataset split
#' `r lifecycle::badge("experimental")`
#'
#' @param tts A list containing the train, test, and optionally valid sets (output of `train_test_split`).
#'
#' @return The test set.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(AirPassengers)
#'   split_data <- dts_train_test_split(AirPassengers, prop = 0.8)
#'   test_set <- dts_test_split(split_data)
#'   test_set
#' }
dts_test_split <- function(tts) {
  tts[["test"]]
}
