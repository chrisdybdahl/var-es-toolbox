library(xts)
library(quarks)
library(rugarch)

forecast_u_HS_var <- function(data, c, n, m) {
  df <- tail(data$Return, n + m)

  res <- rollcast(x = df,
                  p = 1 - c,
                  method = "plain",
                  nout = n,
                  nwin = m
  )
  VaR <- xts(res$VaR, order.by = tail(data$Date, n), colnames = "VaR")
  ES <- xts(res$ES, order.by = tail(data$Date, n), colnames = "ES")
  results_xts <- merge(VaR, ES)
  
  return(results_xts)
}

forecast_u_FHS_EWMA_var <- function(data, c, n, m, b = 10000) {
  df <- tail(data$Return, n + m)

  res <- rollcast(x = df,
                  p = 1 - c,
                  model = "EWMA",
                  method = "fhs",
                  nout = n,
                  nwin = m,
                  nboot = b
  )
  VaR <- xts(res$VaR, order.by = tail(data$Date, n), colnames = "VaR")
  ES <- xts(res$ES, order.by = tail(data$Date, n), colnames = "ES")
  results_xts <- merge(VaR, ES)
  
  return(results_xts)
}

forecast_u_FHS_GARCH_var <- function(data, c, n, m, p = 1, q = 1, b = 10000, model = "sGARCH") {
  df <- tail(data$Return, n + m)

  mean.model <- list(armaOrder = c(0, 0), include.mean = FALSE)
  variance.model <- list(model = model, garchOrder = c(p, q))

  res <- rollcast(x = df,
                  p = 1 - c,
                  model = "GARCH",
                  method = "fhs",
                  nout = n,
                  nwin = m,
                  nboot = b,
                  variance.model = variance.model,
                  mean.model = mean.model
  )
  VaR <- xts(res$VaR, order.by = tail(data$Date, n), colnames = "VaR")
  ES <- xts(res$ES, order.by = tail(data$Date, n), colnames = "ES")
  results_xts <- merge(VaR, ES)
  
  return(results_xts)
}
