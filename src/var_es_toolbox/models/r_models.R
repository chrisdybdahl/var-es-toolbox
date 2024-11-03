library(rugarch)

forecast_ugarch <- function(data, p, q, n) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(p, q)),
                     variance.model = list(model = "sGARCH"),
                     distribution.model = "std")
  fit <- ugarchfit(spec = spec, data = data)
  forecast <- ugarchforecast(fit, n.ahead = n)
  return(forecast)
}
