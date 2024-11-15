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
  residuals <- residuals(fit, standardize = TRUE)
  sigma <- sigma(fit)

  return(list(residuals = residuals, sigma = sigma, fit = fit))
}

fit_POT_EVT <- function(residuals, c, t) {
  u <- quantile(residuals, t)
  # Time-series approach
  exceedances <- (residuals - u) * (residuals > u)

  if (length(exceedances) < 5) {
    warning("Not enough exceedances for EVT fitting. Skipping EVT adjustment.")
    return(NA)
  }

  evt_fit <- gpd(exceedances, threshold = u)
  xi <- evt_fit$par.ests["xi"]
  beta <- evt_fit$par.ests["beta"]

  N <- length(residuals)
  n_u <- length(exceedances)

  # Calculate EVT VaR
  var_evt <- u + (beta / xi) * (((N / n_u) * (1 - c))^(-xi) - 1)
  return(var_evt)
}

forecast_u_EVT_GARCH_var_2 <- function(
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

    var_evt <- fit_POT_EVT(residuals, t, c)

    if (!is.na(var_evt)) {
      warning("VaR_EVT, VaR before conditional volatility adjustment, was NA.")
      # TODO: Assume mu is zero
      var[i] <- -sigmaFor * var_evt
    } else {
      var[i] <- NA
    }
  }

  var <- xts(var, order.by = tail(data$Date, n))

  return(var)
}

forecast_u_EVT_GARCH_var <- function(
  data,
  c,
  n,
  m,
  t = 0.95,
  p = 1,
  q = 1,
  r = 1,
  model = "sGARCH",
  dist = "norm",
  cluster = NULL
) {
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
  realized <- garch_roll@forecast$density$Realized

  standardized_residuals <- (realized - muFor) / sigmaFor

  # Peak over Thresholds, time-series approach
  threshold <- quantile(standardized_residuals, t)
  exceedances <- (standardized_residuals - threshold) * (standardized_residuals > threshold)

  if (length(exceedances) < 5) {
    warning("Not enough exceedances for EVT fitting, skipping EVT adjustment.")
    var_evt <- 0
  } else {
    evt_fit <- gpd(exceedances, threshold = t)
    xi <- evt_fit$par.ests["xi"]
    beta <- evt_fit$par.ests["beta"]

    # EVT quantile adjustment
    N <- length(standardized_residuals)
    n_exceed <- length(exceedances)
    var_evt <- threshold + (beta / xi) * (((N / n_exceed) * (1 - c))^(-xi) - 1)
  }

  # TODO: Assume mu is zero
  var <- -sigmaFor * var_evt

  var <- xts(var, order.by = tail(data$Date, n))

  return(var)
}