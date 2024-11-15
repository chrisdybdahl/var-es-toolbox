library(DEoptim)

forecast_u_CAViaR_var <- function(data, c, n, m, r = 20, model_type = "adaptive", G = 10, itermax = 100, verbose = FALSE) {
  df <- tail(data, n + m)
  data_xts <- xts(df$Return, order.by = df$Date)
  model_function <- switch(model_type,
                           "adaptive" = Adaptive,
                           "symmetricAbs" = SymmetricAbs,
                           "asymmetricSlope" = AsymetricSlope,
                           "indirectGARCH" = IndirectGARCH,
                           "linearGARCH" = linearGARCH,
                           "linearTGARCH" = linearTGARCH,
                           "GJRGARCH" = GJRGARCH,
                           stop("Invalid model type"))

  nparms <- switch(model_type,
                   "adaptive" = 1,
                   "symmetricAbs" = 3,
                   "asymmetricSlope" = 4,
                   "indirectGARCH" = 3,
                   "linearGARCH" = 3,
                   "linearTGARCH" = 4,
                   "GJRGARCH" = 4)

  G <- G

  var <- numeric(n)
  for (i in 1:n) {
    window_start <- i
    window_end <- m + i - 1
    window_xts <- data_xts[window_start:window_end]

    if (i %% r == 0 || i == 1) {
      optim_function <- quantile_loss_function(window_xts, c, betas, model_function)

      result <- DEoptim(
        optim_function,
        lower = rep(-10, nparms),
        upper = rep(10, nparms),
        control = list(trace = verbose, itermax = itermax)
      )
      betas <- result$optim$bestmem
    }

    var[i] <- model_function(window_xts, c, betas)[m]
  }

  var <- -xts(var, order.by = tail(data$Date, n))

  return(var)
}

quantile_loss_function <- function(data, c, betas, func) {
  function(betas) {
    Q <- func(data, c, betas)
    res <- sum(c * abs(data - Q) * (data > Q)) + sum((1 - c) * abs(data - Q) * (data < Q))
    if (is.na(res) | is.infinite(res)) res <- 1e+10
    return(res)
  }
}

# Specifications from Engle and Manganelli (2004)

SymmetricAbs <- function(data, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(data)
  var <- as.numeric(quantile(data, probs = c))
  var <- rep(var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * abs(data[i - 1])
  }
  return(var)
}

AsymetricSlope <- function(data, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- lenth(data)
  var <- as.numeric(quantile(data, probs = c))
  var <- rep(var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * max(data[i - 1], 0) +
      beta4 * min(data[i - 1], 0)
  }
  return(var)
}

IndirectGARCH <- function(data, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(data)
  var <- as.numeric(quantile(data, probs = c))
  var <- rep(var, N)
  for (i in 2:N) {
    var[i] <- (1 - 2 * (c < 0.5)) * (
      beta1 +
      beta2 * (var[i - 1]^2) +
      beta3 * data[i - 1]^2)^(1 / 2)
  }
  return(var)
}

Adaptive <- function(data, c, betas) {
  beta1 <- betas[1]
  var <- as.numeric(quantile(data, probs = c))
  var <- rep(var, length(data))
  for (i in 2:length(data)) {
    var[i] <- var[i - 1] +
      beta1 * (c - ((1 + exp(G * (data[i - 1] - var[i - 1])))^(-1)))
  }
  return(var)
}

# The rest of the models are retreived from PedroBSB/Caviar on GitHub

linearGARCH <- function(data, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  N <- length(data)
  e_var <- as.numeric(quantile(data, probs = c))
  var <- rep(e_var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * abs(data[i - 1])
  }
  return(var)
}

linearTGARCH <- function(data, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(data)
  e_var <- as.numeric(quantile(data, probs = c))
  var <- rep(e_var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * max(data[i - 1], 0) +
      beta4 * min(data[i - 1], 0)
  }
  return(var)
}

GJRGARCH <- function(data, c, betas) {
  beta1 <- betas[1]
  beta2 <- betas[2]
  beta3 <- betas[3]
  beta4 <- betas[4]
  N <- length(data)
  e_var <- as.numeric(quantile(data, probs = c))
  var <- rep(e_var, N)
  for (i in 2:N) {
    var[i] <- beta1 +
      beta2 * var[i - 1] +
      beta3 * data[i - 1] +
      beta4 * data[i - 1] * (data[i - 1] < 0)
  }
  return(var)
}
