library(xts)
library(quarks)
library(quarksNew)
library(rugarch)
library(zoo)

forecast_u_HS <- function(data, c, n, m) {
  df <- tail(data$Return, n + m)

  res <- lapply(c, function(conf) {
    quarks::rollcast(
      x = df,
      p = 1 - conf,
      method = "plain",
      nout = n,
      nwin = m
    )
  })

  # Extract VaR and ES into matrices
  VaR <- do.call(cbind, lapply(res, function(r) r$VaR))
  ES <- do.call(cbind, lapply(res, function(r) r$ES))

  # Format as xts objects
  dates <- tail(data$Date, n)
  VaR <- xts::xts(VaR, order.by = dates)
  ES <- xts::xts(ES, order.by = dates)
  colnames(VaR) <- paste0("VaR_", c)
  colnames(ES) <- paste0("ES_", c)
  results_xts <- xts::merge.xts(VaR, ES)

  return(results_xts)
}

forecast_u_FHS_EWMA <- function(data, c, n, m, lambda = 0.94, b = 10000) {
  df <- tail(data$Return, n + m)

  res <- lapply(c, function(conf) {
    quarks::rollcast(
      x = df,
      p = 1 - conf,
      model = "EWMA",
      method = "fhs",
      lambda = lambda,
      nout = n,
      nwin = m,
      nboot = b
    )
  })

  # Extract VaR and ES into matrices
  VaR <- do.call(cbind, lapply(res, function(r) r$VaR))
  ES <- do.call(cbind, lapply(res, function(r) r$ES))

  # Compute forecasted volatility
  vol_forecast <- numeric(n)
  for (i in 1:n) {
    window_start <- i
    window_end <- m + i - 1
    window <- df[window_start:window_end]
    ewma_var <- quarks::ewma(window, lambda = lambda)
    vol_forecast[i] <- sqrt(ewma_var[length(ewma_var)])
  }

  # Format as xts objects
  dates <- tail(data$Date, n)
  VaR <- xts::xts(VaR, order.by = dates)
  ES <- xts::xts(ES, order.by = dates)
  VOL <- xts::xts(vol_forecast, order.by = dates)
  colnames(VaR) <- paste0("VaR_", c)
  colnames(ES) <- paste0("ES_", c)
  colnames(VOL) <- "VOL"
  results_xts <- xts::merge.xts(VaR, ES, VOL)

  if (any(is.na(results_xts))) {
    warning("There were NA values, carry forward last non-NA")
    results_xts <- zoo::na.locf(results_xts, na.rm = FALSE)
  }

  return(results_xts)
}

forecast_u_FHS_GARCH <- function(data, c, n, m, p = 1, q = 1, b = 10000, model = "sGARCH", dist = "norm", ...) {
  df <- tail(data$Return, n + m)

  mean.model <- list(armaOrder = c(0, 0), include.mean = FALSE)
  variance.model <- list(model = model, garchOrder = c(p, q))

  res <- lapply(c, function(conf) {
    quarksNew::rollcast(
      x = df,
      p = 1 - conf,
      model = "GARCH",
      method = "fhs",
      nout = n,
      nwin = m,
      nboot = b,
      variance.model = variance.model,
      mean.model = mean.model,
      distribution.model = dist,
      solver = "hybrid"
    )
  })

  # Extract VaR and ES into matrices
  VaR <- do.call(cbind, lapply(res, function(r) r$VaR))
  ES <- do.call(cbind, lapply(res, function(r) r$ES))

  # Compute the sigma values using ugarchroll
  spec <- rugarch::ugarchspec(
    mean.model = mean.model,
    variance.model = variance.model,
    distribution.model = dist
  )

  garch_roll <- rugarch::ugarchroll(
    spec = spec,
    data = df,
    forecast.length = n,
    window.size = m,
    refit.every = 1, # Since rollcast is implemented by refitting every period
    refit.window = "moving",
    calculate.VaR = FALSE,
    solver = "hybrid"
  )

  sigma <- garch_roll@forecast$density$Sigma

  # Format results as xts objects
  dates <- tail(data$Date, n)
  VaR <- xts::xts(VaR, order.by = dates)
  ES <- xts::xts(ES, order.by = dates)
  VOL <- xts::xts(sigma, order.by = dates)
  colnames(VaR) <- paste0("VaR_", c)
  colnames(ES) <- paste0("ES_", c)
  colnames(VOL) <- "VOL"
  results_xts <- xts::merge.xts(VaR, ES, VOL)

  if (any(is.na(results_xts))) {
    warning("There were NA values, carry forward last non-NA")
    results_xts <- zoo::na.locf(results_xts, na.rm = FALSE)
  }

  return(results_xts)
}
