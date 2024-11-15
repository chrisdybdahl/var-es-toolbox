library(PerformanceAnalytics)
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
  var <- xts(res$VaR, order.by = tail(data$Date, n))
  print(var)

  return(var)
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
  var <- xts(res$VaR, order.by = tail(data$Date, n))

  return(var)
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
  var <- xts(res$VaR, order.by = tail(data$Date, n))

  return(var)
}
