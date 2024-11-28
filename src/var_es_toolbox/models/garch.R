library(rugarch)
library(xts)

forecast_u_GARCH <- function(data, c, n, m, p = 1, q = 1, r = 1, model = "sGARCH", dist = "norm", cluster = NULL) {
  # TODO: Can remove?
  df <- tail(data, n + m)
  data_xts <- xts(df$Return, order.by = df$Date)

  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    variance.model = list(garchOrder = c(p, q), model = model),
    distribution.model = dist
  )

  garch_roll <- ugarchroll(
    spec = spec,
    data = data_xts,
    forecast.length = n,
    window.size = m,
    refit.every = r,
    refit.window = 'moving',
    calculate.VaR = FALSE, # TODO: Can be dropped if computed
    VaR.alpha = c,
    cluster = cluster
  )

  muFor <- garch_roll@forecast$density$Mu
  sigmaFor <- garch_roll@forecast$density$Sigma

  # Assume mean is zero
  # TODO: Assume mean is zero, have option?
  muFor <- 0

  if (dist == "norm") {
    quantile <- qnorm(c)
    pdf_value <- dnorm(quantile)
    es <- -(muFor - sigmaFor * (pdf_value / c))

  } else if (dist == "std") {
    shapeFor <- garch_roll@forecast$density$Shape
    quantile <- qdist("std", p = c, mu = 0, sigma = 1, shape = shapeFor)
    pdf_value <- ddist("std", quantile, mu = 0, sigma = 1, shape = shapeFor)
    es <- -(muFor - sigmaFor *
      ((shapeFor + quantile^2) / (shapeFor - 1)) *
      (pdf_value / c))

  } else if (dist == "sstd") {
    shapeFor <- garch_roll@forecast$density$Shape
    skewFor <- garch_roll@forecast$density$Skew
    quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
    pdf_value <- ddist("sstd", quantile, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
    es <- -(muFor - sigmaFor *
      ((shapeFor + quantile^2) / (shapeFor - 1)) *
      (pdf_value / c))

  } else {
    stop("Unsupported distribution type. Use 'norm', 'std', or 'sstd'.")
  }

  var <- -(muFor + sigmaFor * quantile)
  # var_auto <- - garch_roll@forecast$VaR[[1]]

  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR <- xts(as.numeric(var), order.by = dates, colnames = "VaR")
  ES <- xts(as.numeric(es), order.by = dates, colnames = "ES")
  VOL <- xts(as.numeric(sigmaFor), order.by = dates, colnames = "VOL")
  results_xts <- merge(VaR, ES, VOL)

  return(results_xts)
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
