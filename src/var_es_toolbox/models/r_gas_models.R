library(GAS)
library(xts)

forecast_u_GAS_var <- function(data, c, n, m, r = 1, dist = "norm", cluster = NULL) {
  df <- tail(data, n + m)
  data_xts <- xts(df$Return, order.by = df$Date)

  GASSpec <- UniGASSpec(
    Dist = dist,
    ScalingType = "Identity",
    GASPar = list(scale = TRUE)
  )

  gas_roll <- UniGASRoll(
    data = data_xts,
    GASSpec = GASSpec,
    ForecastLength = n,
    RefitEvery = r,
    RefitWindow = "moving",
    cluster = cluster
  )

  muFor <- gas_roll@Forecast$PointForecast$location
  sigmaFor <- gas_rsoll@Forecast$PointForecast$scale

  # Assume mean is zero
  # TODO: Assume mean is zero, have option?
  muFor <- 0

  if (dist == "norm") {
    quantile <- qnorm(c)
    pdf_value <- dnorm(quantile)
    es <- - (muFor - sigmaFor * (pdf_value / c))

  } else if (dist == "std") {
    shapeFor <- gas_roll@Forecast$PointForecast$shape
    quantile <- qdist("std", p = c, mu = 0, sigma = 1, shape = shapeFor)
    pdf_value <- ddist("std", quantile, mu = 0, sigma = 1, shape = shapeFor)
    es <- - (muFor - sigmaFor *
      ((shapeFor + quantile^2) / (shapeFor - 1)) *
      (pdf_value / c))

  } else if (dist == "sstd") {
    shapeFor <- gas_roll@Forecast$PointForecast$shape
    skewFor <- gas_roll@Forecast$PointForecast$skew
    quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
    pdf_value <- ddist("sstd", quantile, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
    es <- - (muFor - sigmaFor *
      ((shapeFor + quantile^2) / (shapeFor - 1)) *
      (pdf_value / c))

  } else {
    stop("Unsupported distribution type. Use 'norm', 'std', or 'sstd'.")
  }

  var <- -(muFor + sigmaFor * quantile)

  # TODO: Something strange when plotting
  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR <- xts(as.numeric(var), order.by = dates, colnames = "VaR")
  ES <- xts(as.numeric(es), order.by = dates, colnames = "ES")
  results_xts <- merge(VaR, ES)

  return(results_xts)
}
