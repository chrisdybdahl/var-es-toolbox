library(rugarch)
library(tidyverse)
library(xts)

forecast_u_nGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "sGARCH"),
                     distribution.model = "norm")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_stdGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "sGARCH"),
                     distribution.model = "std")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_sstdGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "sGARCH"),
                     distribution.model = "sstd")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_nAPARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "apARCH"),
                     distribution.model = "norm")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_stdAPARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "apARCH"),
                     distribution.model = "std")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_sstdAPARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "apARCH"),
                     distribution.model = "sstd")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_nEGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "eGARCH"),
                     distribution.model = "norm")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_stdEGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "eGARCH"),
                     distribution.model = "std")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_sstdEGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "eGARCH"),
                     distribution.model = "sstd")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_nGJRGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "gjrGARCH"),
                     distribution.model = "norm")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_stdGJRGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "gjrGARCH"),
                     distribution.model = "std")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_sstdGJRGARCH <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     variance.model = list(garchOrder = c(p, q), model = "gjrGARCH"),
                     distribution.model = "sstd")
  fitMod <- ugarchfit(spec = spec, data = data, out.sample = n)
  forMod <- ugarchforecast(fitMod, n.ahead = 1, n.roll = n - 1)
  volFor <- xts(as.numeric(forMod@forecast$sigmaFor), order.by = tail(data$Date, n))
  return(list(fitMod, forMod, volFor))
}

forecast_u_nGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_nGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  quantile <- qnorm(c)
  varFor <- -sqrt(volFor) * quantile
  return(varFor)
}

forecast_u_stdGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_stdGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  quantile <- qt(c, df = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_sstdGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_sstdGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  skew <- as.numeric(coef(fitMod)["skew"])
  quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skew, shape = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_nAPARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_nAPARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  quantile <- qnorm(c)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_stdAPARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_stdAPARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  quantile <- qt(c, df = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_sstdAPARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_sstdAPARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  skew <- as.numeric(coef(fitMod)["skew"])
  quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skew, shape = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}


forecast_u_nEGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_nEGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  quantile <- qnorm(c)
  var <- -sqrt(volFor) * quantile
  return(var)
}


forecast_u_stdEGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_stdEGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  quantile <- qt(c, df = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}


forecast_u_sstdEGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_sstdEGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  skew <- as.numeric(coef(fitMod)["skew"])
  quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skew, shape = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_nGJRGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_nGJRGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  quantile <- qnorm(c)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_stdGJRGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_stdGJRGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  quantile <- qt(c, df = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_sstdGJRGARCH_var <- function(data, c, p, q, n) {
  result <- forecast_u_sstdGJRGARCH(data, p, q, n)
  fitMod <- result[[1]]
  forMod <- result[[2]]
  volFor <- result[[3]]
  shape <- as.numeric(coef(fitMod)["shape"])
  skew <- as.numeric(coef(fitMod)["skew"])
  quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skew, shape = shape)
  var <- -sqrt(volFor) * quantile
  return(var)
}

forecast_u_GARCH_var <- function(data, c, n, m, p = 1, q = 1, r = 1, model = "sGARCH", dist = "norm", cluster = NULL) {
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    variance.model = list(garchOrder = c(p, q), model = model),
    distribution.model = dist
  )

  df <- tail(data, n + m)
  data_xts <- xts(df$Return, order.by = df$Date)

  garch_roll <- ugarchroll(
    spec = spec,
    data = data_xts,
    forecast.length = n,
    window.size = m,
    refit.every = r,
    refit.window = 'moving',
    calculate.VaR = TRUE,
    VaR.alpha = c,
    cluster = cluster
  )

  muFor <- garch_roll@forecast$density$Mu
  sigmaFor <- garch_roll@forecast$density$Sigma

  if (dist == "norm") {
    quantile <- qnorm(c)
  } else if (dist == "std") {
    shapeFor <- garch_roll@forecast$density$Shape
    quantile <- qdist("std", p = c, mu = 0, sigma = 1, shape = shapeFor)
  } else if (dist == "sstd") {
    shapeFor <- garch_roll@forecast$density$Shape
    skewFor <- garch_roll@forecast$density$Skew
    quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
  } else {
    stop("Unsupported distribution type. Use 'std', 'norm', or 'sstd'.")
  }

  var <- -(muFor + sigmaFor * quantile)
  # var <- - garch_roll@forecast$VaR[[1]]

  return(var)
}

# library(rmgarch)
# spec = gogarchspec(mean.model = list(armaOrder = c(0, 0),
#     include.mean =FALSE),
#     variance.model = list(model = "sGARCH",
#     garchOrder = c(1,1)) ,
#     distribution.model =  "mvnorm"
# )
# fit = gogarchfit(spec = spec, data = y)
# show(fit)
