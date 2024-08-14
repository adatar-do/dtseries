get_arima_comp <- function(model, .comp = NULL){
  .arma <- model[['arma']]
  # arma: AR MA SAR SMA FREQ d D
  .ar <- .arma[[1]]
  .ma <- .arma[[2]]
  .d <- .arma[[6]]
  .sar <- .arma[[3]]
  .sma <- .arma[[4]]
  .D <- .arma[[7]]
  res <- list(
    full = .arma,
    ar = .ar,
    ma = .ma,
    sar = .sar,
    sma = .sma,
    freq = .arma[[5]],
    d = .d,
    D = .D,
    arma = c(.ar, .ma),
    arima = c(.ar, .d, .ma),
    sarma = c(.sar, .sma),
    sarima = c(.sar, .D, .sma),
    drift = tryCatch({!is.null(model[["coef"]][["drift"]])}, error = function(e){FALSE}),
    mean = tryCatch({!is.null(model[["coef"]][["intercept"]])}, error = function(e){FALSE})
  )
  if(is.null(.comp)){
    res
  } else {
    res[[.comp]]
  }
}
