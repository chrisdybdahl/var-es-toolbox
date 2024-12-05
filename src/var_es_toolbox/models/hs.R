library(xts)
library(quarks)
library(rugarch)

forecast_u_HS <- function(data, c, n, m) {
  df <- tail(data$Return, n + m)

  res <- quarks::rollcast(x = df,
                  p = 1 - c,
                  method = "plain",
                  nout = n,
                  nwin = m
  )
  VaR <- xts(res$VaR, order.by = tail(data$Date, n), colnames = paste0("VaR_", c))
  ES <- xts(res$ES, order.by = tail(data$Date, n), colnames = paste0("ES_", c))
  results_xts <- merge(VaR, ES)

  return(results_xts)
}

forecast_u_FHS_EWMA <- function(data, c, n, m, lambda = 0.94, b = 10000) {
  df <- tail(data$Return, n + m)

  res <- quarks::rollcast(x = df,
                  p = 1 - c,
                  model = "EWMA",
                  method = "fhs",
                  lambda = lambda,
                  nout = n,
                  nwin = m,
                  nboot = b
  )

  # Re-computing the sigma used in the rollcast function, since it is not provided
  vol_forecast <- numeric(n)
  for (i in 1:n) {
    # Retrieve the rolling window
    window_start <- i
    window_end <- m + i - 1
    window <- df[window_start:window_end]

    # Compute EWMA variance and volatility
    ewma_var <- quarks::ewma(window, lambda = lambda)
    vol_forecast[i] <- sqrt(ewma_var[length(ewma_var)])  # Forecast volatility
  }

  VaR <- xts::xts(res$VaR, order.by = tail(data$Date, n), colnames = "VaR")
  ES <- xts::xts(res$ES, order.by = tail(data$Date, n), colnames = "ES")
  VOL <- xts::xts(vol_forecast, order.by = tail(data$Date, n), colnames = "VOL")
  results_xts <- merge(VaR, ES, VOL)
  
  return(results_xts)
}

forecast_u_FHS_GARCH <- function(data, c, n, m, p = 1, q = 1, b = 10000, model = "sGARCH", dist = "norm", ...) {
  df <- tail(data$Return, n + m)

  mean.model <- list(armaOrder = c(0, 0), include.mean = FALSE)
  variance.model <- list(model = model, garchOrder = c(p, q))

  res <- quarks::rollcast(x = df,
                  p = 1 - c,
                  model = "GARCH",
                  method = "fhs",
                  nout = n,
                  nwin = m,
                  nboot = b,
                  variance.model = variance.model,
                  mean.model = mean.model,
                  distribution.model = dist
  )

  # Re-computing the sigma used in the rollcast function, since it is not provided
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
    calculate.VaR = FALSE
  )

  VaR <- xts(res$VaR, order.by = tail(data$Date, n), colnames = "VaR")
  ES <- xts(res$ES, order.by = tail(data$Date, n), colnames = "ES")
  VOL <- xts(as.numeric(garch_roll@forecast$density$Sigma), order.by = tail(data$Date, n), colnames = "VOL")
  results_xts <- merge(VaR, ES, VOL)
  
  return(results_xts)
}
