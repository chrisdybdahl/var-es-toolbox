library(DEoptim)

forecast_u_CAViaR_var <- function(data, c, n, m, r = 10, var_model = "adaptive", es_model = "mult", itermax = 200, verbose = FALSE, lb = -10, ub = 10, loss_func = al_log_loss_function, loss_type = "mse") {
  # TODO: Fix how G is parsed to the adaptive model
  df <- tail(data$Return, n + m)
  var_func <- switch(var_model,
                     "adaptive" = Adaptive,
                     "symmetricAbs" = SymmetricAbs,
                     "asymmetricSlope" = AsymetricSlope,
                     "indirectGARCH" = IndirectGARCH,
                     "linearGARCH" = linearGARCH,
                     "linearTGARCH" = linearTGARCH,
                     "GJRGARCH" = GJRGARCH,
                     stop("Invalid VaR model"))

  n_betas <- switch(var_model,
                    "adaptive" = 1,
                    "symmetricAbs" = 3,
                    "asymmetricSlope" = 4,
                    "indirectGARCH" = 3,
                    "linearGARCH" = 3,
                    "linearTGARCH" = 4,
                    "GJRGARCH" = 4)

  lower_beta <- switch(var_model,
                      "indirectGARCH" = 0,
                      lb)

  es_func <- switch(es_model,
                    "mult" = mult_ES,
                    "ar" = ar_ES,
                     stop("Invalid ES model"))

  n_gammas <- switch(es_model,
                     "mult" = 1,
                     "ar" = 3)

  lower_gamma <- switch(es_model,
                        "mult" = lb,
                        "ar" = 0)

  # Record total number of parameters
  n_params <- n_betas + n_gammas

  # Ensure the bounds are met
  lower <- c(rep(lower_beta, n_betas), rep(lower_gamma, n_gammas))
  upper <- rep(ub, n_params)

  # Temporarily setting a random seed
  # set.seed(0)

  # Initialize variables before iterating for rolling window
  var <- numeric(n)
  es <- numeric(n)
  last_params <- NULL
  for (i in 1:n) {
    # Retrieve the rolling window
    window_start <- i
    window_end <- m + i - 1
    window_xts <- df[window_start:window_end]

    # Reparametrize for every r period
    if (i %% r == 0 || i == 1) {
      optim_function <- loss_func(window_xts, c, var_func, es_func, n_betas, loss_type = loss_type)

      # Record the parameters from last iteration
      if (FALSE) {
        control <- list(trace = verbose, itermax = itermax, initialpop = last_params)
      } else {
        control <- list(trace = verbose, itermax = itermax)
      }

      # Optimize the given loss function wrt. betas and gammas
      result <- DEoptim(
        optim_function,
        lower = lower,
        upper = upper,
        control = control
      )

      # Record parameters
      last_params <- result$optim$bestmem
    }

    # Get parameters
    betas <- last_params[1:n_betas]
    gammas <- last_params[n_betas + 1:n_params]

    # Forecast VaR for next timestep
    var[i] <- var_func(window_xts, c, betas)[m]

    # Initialize vectors for current and next timestep for y, VaR, x
    if (i == 1) {
      var_input <- c(0, var[i])
      x_input <- c(0, 0)
    } else {
      var_input <- var[(i-1):i]
      x_input <- c(var[i - 1] - es[i - 1], 0)
    }

    y_input <- c(window_xts[m], 0)

    es_output <- switch(es_model,
                        "mult" = es_func(y_input, var_input, gammas),
                        "ar" = es_func(y_input, var_input, gammas, x_input))
    es[i] <- es_output[2]
  }

  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR <- xts(-as.numeric(var), order.by = dates, colnames = "VaR")
  ES <- xts(-as.numeric(es), order.by = dates, colnames = "ES")
  results_xts <- merge(VaR, ES)

  return(results_xts)
}

# ES-CAViaR using multiple of VaR, described by Taylor (2019)
mult_ES <- function(y, Q, gammas, x = NULL) {
  gamma0 <- gammas[1]
  es <- (1 + exp(gamma0)) * Q

  return(es)
}

# ES-CAViaR using an AR model of ES, described by Taylor (2019)
ar_ES <- function(y, Q, gammas, x = NULL) {
  gamma0 <- gammas[1]
  gamma1 <- gammas[2]
  gamma2 <- gammas[3]
  t <- length(Q)

  calc_x <- function(y, Q, x, t, gamma0, gamma1, gamma2) {
    if (y[t - 1] <= Q[t - 1]) {
      x_t <- gamma0 +
        gamma1 * (Q[t - 1] - y[t - 1]) +
        gamma2 * x[t - 1]
    } else {
      x_t <- x[t - 1]
    }

    return(x_t)
  }

  n <- length(Q)
  # If x is given, then do not calculate x recursively
  if (!is.null(x)) {
    x[n] <- calc_x(y, Q, x, n, gamma0, gamma1, gamma2)
  } else {
    x <- numeric(n)
    for (t in 2:n) {
      x[t] <- calc_x(y, Q, x, t, gamma0, gamma1, gamma2)
    }
  }

  es <- Q - x

  return(es)
}

# TODO: Implemented AL log score function, remember that W * Q_t < ES_t
# Taylor (2019), with proof from Acerbi and Székeley (2014)
# Negative Asymmetric Laplace Log-Likelihood Score Function is strictly consistent for joint VaR and ES when
# the following is set in the fz_general_score_function: 'G_1(x) = -(W/2)x^2', 'G_2(x) = c * x', 'a = 0'
# AND where W is a constant such that 'W * Q_t < ES_t' - here W = 4
al_log_loss_function <- function(y, c, func_Q, func_ES, n_betas, loss_type = "mean") {
  function(params) {
    n_params <- length(params)
    betas <- params[1:n_betas]
    gammas <- params[(n_betas + 1):n_params]

    Q <- func_Q(y, c, betas)
    ES <- func_ES(y, Q, gammas)

    # Calculate the loss vector
    loss_vector <- -log((c - 1) / ES) -
      ((y - Q) * (c - (y <= Q))) / (c * ES) # '+ y / ES' is omitted, Taylor showed this is still strictly consistent

    # Handle NA or infinite values to prevent errors in optimization
    loss_vector[is.na(loss_vector) | is.infinite(loss_vector)] <- 1e+10

    # Aggregate the loss based on the chosen method
    total_loss <- switch(
      loss_type,
      "sum" = sum(loss_vector),
      "mean" = mean(loss_vector),
      "sum_squared" = sum(loss_vector^2),
      "mse" = mean(loss_vector^2),
      mean(loss_vector)
    )

    return(total_loss)
  }
}


# Fissler and Ziegel (2016)
# Strictly consistent scoring functions for jointly evaluating VaR and ES forecasts are on the following form
# func_G1 is strictly increasing, func_G2 = zeta2', zeta2 is increasing and convex
fz_general_score_function <- function(func_Q, func_ES, func_G1, func_G2, func_zeta2, func_a, n_betas) {
  function(y, c, params) {
    n_params <- length(params)
    betas <- params[1:n_betas]
    gammas <- params[(n_betas + 1):n_params]

    Q <- func_Q(y, c, betas)
    ES <- func_ES(y, c, gammas)

    term1 <- ((y <= Q) - c) * func_G1(Q)
    term2 <- -(y <= Q) * func_G1(y)
    term3 <- func_G2(ES) * (ES - Q + (y <= Q) * (Q - y) / c)
    term4 <- -func_zeta2(ES)
    term5 <- func_a(y)

    score <- term1 + term2 + term3 + term4 + term5

    return(score)
  }
}

quantile_loss_function <- function(y, c, func) { # TODO: Check if need betas in argument
  function(betas) {
    Q <- func(y, c, betas)
    res <- sum(c * abs(y - Q) * (y > Q)) + sum((1 - c) * abs(y - Q) * (y < Q))
    if (is.na(res) | is.infinite(res)) res <- 1e+10
    return(res)
  }
}

# Specifications from Engle and Manganelli (2004)

SymmetricAbs <- function(y, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(y)
  var <- as.numeric(quantile(y, probs = c))
  var <- rep(var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * abs(y[i - 1])
  }
  return(var)
}

AsymetricSlope <- function(y, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(y)
  var <- as.numeric(quantile(y, probs = c))
  var <- rep(var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * max(y[i - 1], 0) +
      beta4 * min(y[i - 1], 0)
  }
  return(var)
}

IndirectGARCH <- function(y, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(y)
  var <- as.numeric(quantile(y, probs = c))
  var <- rep(var, N)
  for (i in 2:N) {
    var[i] <- (1 - 2 * (c < 0.5)) * (
      beta1 +
        beta2 * (var[i - 1]^2) +
        beta3 * y[i - 1]^2)^(1 / 2)
  }
  return(var)
}

Adaptive <- function(y, c, betas, G = 1) {
  beta1 <- betas[1]
  var <- as.numeric(quantile(y, probs = c))
  var <- rep(var, length(y))
  for (i in 2:length(y)) {
    var[i] <- var[i - 1] +
      beta1 * (c - ((1 + exp(G * (y[i - 1] - var[i - 1])))^(-1)))
  }
  return(var)
}

# The rest of the models are retreived from PedroBSB/Caviar on GitHub

linearGARCH <- function(y, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(y)
  e_var <- as.numeric(quantile(y, probs = c))
  var <- rep(e_var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * abs(y[i - 1])
  }
  return(var)
}

linearTGARCH <- function(y, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(y)
  e_var <- as.numeric(quantile(y, probs = c))
  var <- rep(e_var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * max(y[i - 1], 0) +
      beta4 * min(y[i - 1], 0)
  }
  return(var)
}

GJRGARCH <- function(y, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(y)
  e_var <- as.numeric(quantile(y, probs = c))
  var <- rep(e_var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * y[i - 1] +
      beta4 * y[i - 1] * (y[i - 1] < 0)
  }
  return(var)
}
