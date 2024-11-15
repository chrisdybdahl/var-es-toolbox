library(rugarch)
library(xts)

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
