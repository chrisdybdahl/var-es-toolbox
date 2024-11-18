library(rugarch)
library(xts)
library(evir)

fit_GARCH <- function(
  data,
  p = 1,
  q = 1,
  model = "sGARCH",
  dist = "norm",
  solver = "solnp",
  solver.control = list(n.restarts = 1)
) {
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    variance.model = list(garchOrder = c(p, q), model = model),
    distribution.model = dist
  )

  fit <- ugarchfit(
    spec = spec,
    data = data,
    solver = solver,
    solver.control = solver.control
  )
  sigma <- rugarch::sigma(fit)
  residuals <- rugarch::residuals(fit, standardize = TRUE)

  return(list(residuals = residuals, sigma = sigma, fit = fit))
}

fit_POT_EVT <- function(residuals, c, t) {
  # Model the lower tail
  residuals <- - residuals

  # Threshold selection
  u <- quantile(residuals, probs = t)

  # Calculate exceedances with a time-series framework
  exceedances <- (residuals - u) * (residuals > u)

  # Calculate exceedances with a marked point process framework
  # exceedances <- residuals[residuals > u] - u

  if (length(exceedances[exceedances > 0]) < 5) {
    warning("Not enough exceedances for EVT fitting. Skipping EVT adjustment.")
    var_evt <- NA
  } else {
    # Fit the Generalized Pareto Distribution
    evt_fit <- evir::gpd(exceedances, threshold = u)
    xi <- evt_fit$par.ests["xi"]
    beta <- evt_fit$par.ests["beta"]

    # EVT quantile adjustment
    N <- length(residuals)
    n_u <- length(exceedances)

    # Using Pickands-Balkema-de Haan Extreme Value Theorem (Balkema & de Haan, 1974) we can derive VaR for GPD
    # From Hull (2018) we have a definition for ES for GPD
    if (xi != 0) {
      var_evt <- u + (beta / xi) * (((N / n_u) * (1 - c))^(-xi) - 1)
      es_evt <- (var_evt + beta - u * xi) / (1 - xi)
    } else if (xi == 0) {
      var_evt <- u - beta * log((N / n_u) * (1 - c))
      es_evt <- var_evt + beta
    } else {
      var_evt <- NA
      es_evt <- NA
      }

  }
  return(list(VaR_evt = var_evt, ES_evt = es_evt))
}

forecast_u_EVT_GARCH_var <- function(
  data,
  c,
  n,
  m,
  t = 0.95,
  p = 1,
  q = 1,
  model = "sGARCH",
  dist = "norm",
  solver = "solnp",
  solver.control = list(n.restarts = 1)
) {
  df <- tail(data, n + m)
  data_xts <- xts(df$Return, order.by = df$Date)

  var <- numeric(n)
  es <- numeric(n)
  for (i in 1:n) {
    window_start <- i
    window_end <- m + i - 1
    window_xts <- data_xts[window_start:window_end]

    garch_result <- fit_GARCH(
      window_xts,
      p = p,
      q = q,
      model = model,
      dist = dist,
      solver = solver,
      solver.control = solver.control
    )

    residuals <- garch_result$residuals
    sigmaFor <- tail(garch_result$sigma, 1)

    evt_result <- fit_POT_EVT(residuals, c, t)

    var_evt <- evt_result$VaR_evt
    es_evt <- evt_result$ES_evt

    if (!is.na(var_evt)) {
      warning("VaR_EVT, VaR before conditional volatility adjustment, was NA.")
      # TODO: Assume mu is zero
      var[i] <- sigmaFor * var_evt
      es[i] <- sigmaFor * es_evt
    } else {
      var[i] <- NA
      es[i] <- NA
    }
  }

  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR <- xts(as.numeric(var), order.by = dates, colnames = "VaR")
  ES <- xts(as.numeric(es), order.by = dates, colnames = "ES")
  results_xts <- merge(VaR, ES)


  return(results_xts)
}
