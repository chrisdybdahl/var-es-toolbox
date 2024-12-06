library(rugarch)
library(xts)
library(evir)

forecast_u_EVT_GARCH <- function(
  data,
  c,
  n,
  m,
  p = 1,
  q = 1,
  r = 1,
  t = 0.9,
  model = "sGARCH",
  dist = "norm",
  min_exceedances = 5,
  ...
) {
  df <- tail(data, n + m)
  data_xts <- xts::xts(df$Return, order.by = df$Date)

  spec <- rugarch::ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    variance.model = list(garchOrder = c(p, q), model = model),
    distribution.model = dist
  )

   # Initialize storage for results
  var <- matrix(NA, nrow = n, ncol = length(c), dimnames = list(NULL, paste0("VaR_", c)))
  es <- matrix(NA, nrow = n, ncol = length(c), dimnames = list(NULL, paste0("ES_", c)))

  vol <- numeric(n)
  xi <- NA
  beta <- NA
  for (i in 1:n) {
    window_start <- i
    window_end <- m + i - 1
    window <- data_xts[window_start:window_end]

    fit <- rugarch::ugarchfit(
      spec = spec,
      data = window,
      ...
    )

    vol[i] <- rugarch::sigma(fit)[m]

    # Reparametrize for every r period
    if (i %% r == 0 || i == 1) {
      std_res <- rugarch::residuals(fit, standardize = TRUE)

      # Threshold selection
      u <- quantile(std_res, probs = t)

      # Calculate exceedances with a marked point process framework
      n_u <- length(std_res[std_res > u])

      if (n_u >= min_exceedances) {
        # Fit the Generalized Pareto Distribution with tryCatch
        evt_fit <- tryCatch({
          evir::gpd(std_res, threshold = u, method = "ml")
        }, error = function(e) {
          warning("GPD fitting failed: ", e$message)
          return(NULL)
        })

        if (!is.null(evt_fit)) {
          # Extract parameters if fitting was successful
          xi <- evt_fit$par.ests["xi"]
          beta <- evt_fit$par.ests["beta"]
        } else {
          # Handle failed fitting
          warning("GPD fitting was not successful, setting parameters to last value.")
          xi <- x1
          beta <- beta
        }
      } else {
        warning("Not enough exceedances for EVT fitting. Skipping EVT adjustment.")
        xi <- x1
        beta <- beta
      }
    }

    if (!any(is.na(c(xi, beta, m / n_u)))) {
      # Using Pickands-Balkema-de Haan Extreme Value Theorem (Balkema & de Haan, 1974) we can derive VaR for GPD
      # From Hull (2018) we have a definition for ES for GPD

      var_evt <- ifelse(
        xi != 0,
        u + (beta / xi) * (((m / n_u) * c)^(-xi) - 1),
        u - beta * log((m / n_u) * c)
      )

      es_evt <- ifelse(
        xi != 0,
        (var_evt + beta - u * xi) / (1 - xi),
        var_evt + beta
      )

      # Store results for each confidence level
      var[i, ] <- vol[i] * var_evt
      es[i, ] <- vol[i] * es_evt
    } else {
      warning("xi, beta, or m / n_u was NA for confidence level ", conf)
      var[i, ] <- NA
      es[i, ] <- NA
    }
  }

  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR_xts <- xts::xts(var, order.by = dates)
  ES_xts <- xts::xts(es, order.by = dates)
  VOL <- xts::xts(vol, order.by = dates, colnames = "VOL")
  results_xts <- merge(VaR_xts, ES_xts, VOL)

  return(results_xts)
}