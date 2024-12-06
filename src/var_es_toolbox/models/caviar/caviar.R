library(DEoptim)
library(GAS)
library(Rcpp)

# TODO: Improve
fSourceLocal <- function() {

  # Function to return sourced file location inside to
  # be used inside caviarOptim, where sourceCpp works

  # I do know that it is not well written and precise,
  # but for the sake that the function is only in one file,
  # and will be used within other scripts, I decided the issue
  # will be tackled with that way
  #
  # Moreover this is in any way a package or a class, hence
  # among tools that I know this was the only one

  for (i in -(1:sys.nframe())) {
    if (identical(sys.function(i), base::source)) {
      path <- normalizePath(sys.frame(i)$ofile)
    }
  }

  # if path found return it, else NULL
  if (exists("path")) {
    return(dirname(path))
  } else {
    return(NULL)
  }
}

curr_dir <- fSourceLocal()

forecast_u_CAViaR <- function(
  data,
  c,
  n,
  m,
  r = 10,
  var_model = "ADAPTIVE",
  es_model = "MULT",
  loss = "AL",
  itermax = 250,
  lb = -5,
  ub = 5,
  verbose = FALSE,
  ...
) {
  # TODO: Fix how G is parsed to the adaptive model
  # TODO: Change add some checks

  sourceCpp(paste0(curr_dir, "/" , var_model, ".cpp"))

  # Initialize variables for var_model
  if (var_model == "ADAPTIVE") {
    var_func <- Adaptive_C
    n_betas <- 1
    lower_beta <- lb
  } else if (var_model == "SAV") {
    var_func <- SAV_C
    n_betas <- 3
    lower_beta <- lb
  } else if (var_model == "AS") {
    var_func <- AS_C
    n_betas <- 4
    lower_beta <- lb
  } else if (var_model == "indirectGARCH") {
    var_func <- indirectGARCH_C
    n_betas <- 3
    lower_beta <- 0
  } else if (var_model == "linearGARCH") {
    var_func <- linearGARCH_C
    n_betas <- 3
    lower_beta <- lb
  } else if (var_model == "linearTGARCH") {
    var_func <- linearTGARCH_C
    n_betas <- 4
    lower_beta <- lb
  } else if (var_model == "gjrGARCH") {
    var_func <- gjrGARCH_C
    n_betas <- 4
    lower_beta <- lb
  } else {
    stop("Invalid VaR model")
  }

  sourceCpp(paste0(curr_dir, "/" , "ES_", es_model, ".cpp"))

  # Initialize variables for es_model
  if (es_model == "MULT") {
    es_func <- mult_ES_C
    n_gammas <- 1
    lower_gamma <- lb
  } else if (es_model == "AR") {
    es_func <- ar_ES_C
    n_gammas <- 3
    lower_gamma <- 0
  } else {
    stop("Invalid ES model")
  }

  sourceCpp(paste0(curr_dir, "/" , "ObjectiveFunction.cpp"))

  if (loss == "AL") {
    loss_func <- al_log_loss_function_C
  } else {
    stop("Invalid loss function")
  }

  # Record total number of parameters
  n_params <- n_betas + n_gammas

  # Ensure the bounds are met
  lower <- c(rep(lower_beta, n_betas), rep(lower_gamma, n_gammas))
  upper <- rep(ub, n_params)

  # Initialize variables before iterating for rolling window
  df <- tail(data$Return, n + m)
  var <- numeric(n)
  es <- numeric(n)
  last_params <- NULL
  last_population <- NULL
  for (i in 1:n) {
    # Retrieve the rolling window
    window_start <- i
    window_end <- m + i - 1
    window <- df[window_start:window_end]

    # Reparametrize for every r period
    if (i %% r == 0 || i == 1) {
      optim_function <- objective_handler(window, c, var_func, es_func, n_betas, loss_func)

      # Optimize the given loss function wrt. betas and gammas
      result <- DEoptim::DEoptim(
        optim_function,
        lower = lower,
        upper = upper,
        control = list(
          trace = verbose,
          itermax = itermax,
          initialpop = if (!is.null(last_population)) last_population else NULL,
          ...)
      )

      # Record parameters
      last_params <- result$optim$bestmem

      # Save the last population for the next iteration
      last_population <- result$member$pop
    }

    # Get parameters
    betas <- last_params[1:n_betas]
    gammas <- last_params[n_betas + 1:n_params]

    # Forecast VaR for next timestep
    u <- quantile(window, probs = c)
    var[i] <- var_func(y = window, betas = betas, u = u, c = c)[m]

    # Initialize vectors for current and next timestep for y, x, and Q - which is the empirical quantile first iteration
    Q_input <- if (i == 1) c(u, var[i]) else var[(i - 1):i]
    y_input <- c(window[m], 0)
    if (es_model == "ar") {
      x_input <- if (i == 1) rep(0, 2) else c(var[i - 1] - es[i - 1], 0)
      es_output <- es_func(y = y_input, Q = Q_input, gammas = gammas, x = x_input)
    } else {
      es_output <- es_func(y = y_input, Q = Q_input, gammas = gammas)
    }
    es[i] <- es_output[2]
  }

  # Create xts objects for VaR and ES
  dates <- tail(data$Date, n)
  VaR <- xts(-as.numeric(var), order.by = dates, colnames = "VaR")
  ES <- xts(-as.numeric(es), order.by = dates, colnames = "ES")
  results_xts <- merge(VaR, ES)

  return(results_xts)
}

# TODO: Implemented AL log score function, remember that W * Q_t < ES_t
# Taylor (2019), with proof from Acerbi and Székeley (2014)
# Negative Asymmetric Laplace Log-Likelihood Score Function is strictly consistent for joint VaR and ES when
# the following is set in the fz_general_score_function: "G_1(x) = -(W/2)x^2", "G_2(x) = c * x", "a = 0"
# AND where W is a constant such that "W * Q_t < ES_t" - here W = 4
al_log_loss_function <- function(y, Q, ES, c) {
  loss_vector <- -log((c - 1) / ES) -
    ((y - Q) * (c - (y <= Q))) / (c * ES) # "+ y / ES" is omitted, Taylor showed this is still strictly consistent
  return(loss_vector)
}

objective_handler <- function(y, c, func_Q, func_ES, n_betas, loss_function) {
  function(params) {
    betas <- params[1:n_betas]
    gammas <- params[(n_betas + 1):length(params)]

    u <- quantile(y, probs = c)

    Q <- func_Q(y = y, betas = betas, u = u, c = c)
    ES <- func_ES(y = y, Q = Q, gammas = gammas)

    # Calculate the loss vector
    ES[ES >= 0] <- -1e-10
    loss_vector <- loss_function(y, Q, ES, c)

    # # Handle NA or infinite values to prevent errors in optimization
    loss_vector[is.na(loss_vector) | is.infinite(loss_vector)] <- 1e+10

    return(mean(loss_vector))
  }
}

# Fissler and Ziegel (2016)
# Strictly consistent scoring functions for jointly evaluating VaR and ES forecasts are on the following form
# func_G1 is strictly increasing, func_G2 = zeta2", zeta2 is increasing and convex
fz_general_score_function <- function(func_Q, func_ES, func_G1, func_G2, func_zeta2, func_a, n_betas) {
  function(y, c, params) {
    n_params <- length(params)
    betas <- params[1:n_betas]
    gammas <- params[(n_betas + 1):n_params]

    u <- quantile(y, probs = c)

    Q <- func_Q(y = y, betas = betas, u = u, c = c)
    ES <- func_ES(y = y, Q = Q, gammas = gammas)

    term1 <- ((y <= Q) - c) * func_G1(Q)
    term2 <- -(y <= Q) * func_G1(y)
    term3 <- func_G2(ES) * (ES - Q + (y <= Q) * (Q - y) / c)
    term4 <- -func_zeta2(ES)
    term5 <- func_a(y)

    score <- term1 + term2 + term3 + term4 + term5

    return(score)
  }
}

# ES-CAViaR using multiple of VaR, described by Taylor (2019)
mult_ES <- function(Q, gammas, ...) {
  gamma0 <- gammas[1]
  es <- (1 + exp(gamma0)) * Q

  return(es)
}

# ES-CAViaR using an AR model of ES, described by Taylor (2019)
ar_ES <- function(y, Q, gammas, x = NULL, ...) {
  gamma0 <- gammas[1]
  gamma1 <- gammas[2]
  gamma2 <- gammas[3]

  if (is.null(x)) {
    x <- numeric(length(Q))
    for (t in 2:length(Q)) {
      if (y[t - 1] <= Q[t - 1]) {
        x[t] <- gamma0 +
          gamma1 * (Q[t - 1] - y[t - 1]) +
          gamma2 * x[t - 1]
      } else {
        x[t] <- x[t - 1]
      }
    }
  } else {
    t <- length(Q)
    if (y[t - 1] <= Q[t - 1]) {
      x[t] <- gamma0 +
        gamma1 * (Q[t - 1] - y[t - 1]) +
        gamma2 * x[t - 1]
    } else {
      x[t] <- x[t - 1]
    }
  }

  es <- Q - x

  return(es)
}

# Specifications from Engle and Manganelli (2004)

SAV <- function(y, betas, u, ...) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * abs(y[i - 1])
  }
  return(var)
}

AS <- function(y, betas, u, ...) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * max(y[i - 1], 0) +
      beta4 * min(y[i - 1], 0)
  }
  return(var)
}

indirectGARCH <- function(y, betas, u, c, ...) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:N) {
    var[i] <- (1 - 2 * (c < 0.5)) * (
      beta1 +
        beta2 * (var[i - 1]^2) +
        beta3 * y[i - 1]^2)^(1 / 2)
  }
  return(var)
}

adaptive <- function(y, betas, u, c, G = 10, ...) {
  beta1 <- betas[1]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:length(y)) {
    var[i] <- var[i - 1] +
      beta1 * (c - ((1 + exp(G * (y[i - 1] - var[i - 1])))^(-1)))
  }
  return(var)
}

# The rest of the models are retreived from PedroBSB/Caviar on GitHub

linearGARCH <- function(y, betas, u, ...) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * abs(y[i - 1])
  }
  return(var)
}

linearTGARCH <- function(y, betas, u, ...) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * max(y[i - 1], 0) +
      beta4 * min(y[i - 1], 0)
  }
  return(var)
}

gjrGARCH <- function(y, betas, u, ...) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(y)
  var <- rep(u, N)
  var[1] <- u
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * y[i - 1] +
      beta4 * y[i - 1] * (y[i - 1] < 0)
  }
  return(var)
}
