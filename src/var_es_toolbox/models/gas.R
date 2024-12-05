library(GAS)
library(rugarch)
library(xts)

# TODO: Fix ESR backtests with GAS

forecast_u_GAS <- function(data, c, n, m, r = 1, dist = "norm", ...) {
  df <- tail(data$Return, n + m)
  GASSpec <- GAS::UniGASSpec(
    Dist = dist,
    ScalingType = "Identity",
    GASPar = list(scale = TRUE)
  )

  gas_roll <- GAS::UniGASRoll(
    data = df,
    GASSpec = GASSpec,
    ForecastLength = n,
    RefitEvery = r,
    RefitWindow = "moving",
    ...
  )

  # muFor <- gas_roll@Forecast$PointForecast[, "location"]
  # sigmaFor <- gas_roll@Forecast$PointForecast[, "scale"]
  #
  # # Assume mean is zero
  # # TODO: Assume mean is zero, have option?
  # muFor <- 0
  #
  # if (dist == "norm") {
  #   quantile <- stats::qnorm(c)
  #   pdf_value <- stats::dnorm(quantile)
  #   es <- muFor + sigmaFor * (pdf_value / c)
  #
  # } else if (dist == "std") {
  #   shapeFor <- gas_roll@Forecast$PointForecast[, "shape"]
  #   quantile <- rugarch::qdist(distribution = "std", p = c, mu = 0, sigma = 1, shape = shapeFor)
  #   pdf_value <- rugarch::ddist(distribution = "std", quantile, mu = 0, sigma = 1, shape = shapeFor)
  #   es <- muFor + sigmaFor *
  #     ((shapeFor + quantile^2) / (shapeFor - 1)) *
  #     (pdf_value / c)
  #
  # } else if (dist == "sstd") {
  #   shapeFor <- gas_roll@Forecast$PointForecast[, "shape"]
  #   skewFor <- gas_roll@Forecast$PointForecast[, "skewness"]
  #   quantile <- rugarch::qdist("sstd", p = c, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
  #   pdf_value <- rugarch::ddist("sstd", quantile, mu = 0, sigma = 1, skew = skewFor, shape = shapeFor)
  #   es <- muFor + sigmaFor *
  #     ((shapeFor + quantile^2) / (shapeFor - 1)) *
  #     (pdf_value / c)
  #
  # } else {
  #   stop("Unsupported distribution type. Use 'norm', 'std', or 'sstd'.")
  # }
  #
  # var <- muFor - sigmaFor * quantile

  var <- -GAS::quantile(gas_roll, probs = c)
  es <- -GAS::ES(gas_roll, probs = c)

  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR <- xts::xts(as.numeric(var), order.by = dates, colnames = "VaR")
  ES <- xts::xts(as.numeric(es), order.by = dates, colnames = "ES")
  results_xts <- merge(VaR, ES)

  return(results_xts)
}
