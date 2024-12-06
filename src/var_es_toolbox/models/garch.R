library(rugarch)
library(xts)

forecast_u_GARCH <- function(
  data,
  c,
  n,
  m,
  p = 1,
  q = 1,
  r = 1,
  model = "sGARCH",
  dist = "norm",
  ...
) {
  df <- tail(data, n + m)
  data_xts <- xts::xts(df$Return, order.by = df$Date)

  spec <- rugarch::ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    variance.model = list(garchOrder = c(p, q), model = model),
    distribution.model = dist
  )

  garch_roll <- rugarch::ugarchroll(
    spec = spec,
    data = data_xts,
    forecast.length = n,
    window.size = m,
    refit.every = r,
    refit.window = 'moving',
    ...
  )

  sigmaFor <- garch_roll@forecast$density$Sigma

  # Assume mean is zero
  # TODO: Assume mean is zero
  if (dist == "norm") {
    quantile <- stats::qnorm(c)
    pdf_value <- stats::dnorm(quantile)
    es <- -sigmaFor * (pdf_value / c)
    var <- -sigmaFor * (quantile)
  } else if (dist == "std") {
    shapeFor <- garch_roll@forecast$density$Shape
    quantile <- sapply(c, function(conf) rugarch::qdist("std", p = conf, mu = 0, sigma = 1, shape = shapeFor))
    pdf_value <- sapply(c, function(conf) rugarch::ddist("std", quantile[, which(c == conf)], mu = 0, sigma = 1, shape = shapeFor))
    quantile <- quantile[1:n, , drop = FALSE]
    pdf_value <- pdf_value[1:n, , drop = FALSE]
    es <- -sigmaFor * ((shapeFor + quantile^2) / (shapeFor - 1) * (pdf_value / c))
    var <- -sigmaFor * (quantile)
  } else if (dist == "sstd") {
    shapeFor <- garch_roll@forecast$density$Shape
    skewFor <- garch_roll@forecast$density$Skew
    quantile <- sapply(c, function(conf) rugarch::qdist("sstd", p = conf, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor))
    pdf_value <- sapply(c, function(conf) rugarch::ddist("sstd", quantile[, which(c == conf)], mu = 0, sigma = 1, skew = skewFor, shape = shapeFor))
    quantile <- quantile[1:n, , drop = FALSE]
    pdf_value <- pdf_value[1:n, , drop = FALSE]
    es <- -sigmaFor * ((shapeFor + quantile^2) / (shapeFor - 1) * (pdf_value / c))
    var <- -sigmaFor * (quantile)
  } else {
    stop("Unsupported distribution type. Use 'norm', 'std', or 'sstd'.")
  }

  dates <- tail(data$Date, n)
  VaR <- xts::xts(var, order.by = dates)
  ES <- xts::xts(es, order.by = dates)
  colnames(VaR) <- paste0("VaR_", c)
  colnames(ES) <- paste0("ES_", c)
  VOL <- xts::xts(as.numeric(sigmaFor), order.by = dates, colnames = "VOL")
  results_xts <- merge(VaR, ES, VOL)

  return(results_xts)
}
