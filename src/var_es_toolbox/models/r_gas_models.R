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
  sigmaFor <- gas_roll@Forecast$PointForecast$scale

  if (dist == "norm") {
    quantile <- qnorm(c)
  } else if (dist == "std") {
    shapeFor <- gas_roll@Forecast$PointForecast$shape
    quantile <- qdist("std", p = c, mu = 0, sigma = 1, shape = shapeFor)
  } else if (dist == "sstd") {
    shapeFor <- gas_roll@Forecast$PointForecast$shape
    skewFor <- gas_roll@Forecast$PointForecast$skew
    quantile <- qdist("sstd", p = c, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
  } else {
    stop("Unsupported distribution type. Use 'std', 'norm', or 'sstd'.")
  }

  var <- -(muFor + sigmaFor * quantile)

  return(var)
}
