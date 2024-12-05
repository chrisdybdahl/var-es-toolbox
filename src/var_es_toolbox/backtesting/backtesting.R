library(esback)
library(Rcpp)
library(rugarch)
library(segMGarch)

# Kupiec (1995) and Christoffersen (1998)
run_kc_backtest <- function(actual, VaR, alpha) {
  result <- rugarch::VaRTest(alpha = alpha, actual = actual, VaR = VaR)
  return(list(
    ExpectedExceedances = result$expected.exceed,
    ActualExceedances = result$actual.exceed,
    UC = result$uc.LRp,
    CC = result$cc.LRp
  ))
}

# Christoffersen and Pelletier (2004)
# TODO: Remember the following note
# Following Christoffersen and Pelletier (2004), the Weibull distribution is used with parameter ‘b=1’ representing the
# case of the exponential. A future release will include the choice of using a bootstrap method to evaluate the p-value,
# and until then care should be taken when evaluating series of length less than 1000 as a rule of thumb.
run_vdt_backtest <- function(actual, VaR, alpha) {
  result <- rugarch::VaRDurTest(alpha = alpha, actual = actual, VaR = VaR)
  return(result$LRp)
}

# Dynamic Quantile Test (Engle and Manganelli, 2004)
run_dq_backtest <- function(actual, VaR, alpha, lag = 1, lag_hit = 1, lag_var = 1) {
  result <- segMGarch::DQtest(y = as.numeric(actual), VaR = 1 - VaR, VaR_level = alpha, lag = lag, lag_hit = lag_hit, lag_var = lag_var)
  return(result[[2]])
}

# McNeil and Frey (2000) test
run_er_backtest <- function(actual, VaR, ES, vol = NULL, b = 1000) {
  result <- esback::er_backtest(r = actual, q = VaR, e = ES, s = vol, B = b)
  return(if (is.null(vol)) result$pvalue_twosided_simple else result$pvalue_twosided_standardized)
}

# McNeil and Frey (2000) test second version
run_er_backtest_2 <- function(actual, VaR, ES, alpha, boot = FALSE, b = 1000) {
  result <- rugarch::ESTest(alpha = alpha, actual = actual, VaR = VaR, ES = ES, boot = boot, n.boot = b)
  return(result$p.value)
}

# Nolde and Ziegel (2017) test
run_coc_backtest <- function(actual, VaR, ES, alpha, vol = NULL) {
  result <- cc_backtest(r = actual, q = VaR, e = ES, s = vol, alpha = alpha)
    return(if (is.null(vol)) result$pvalue_twosided_simple else result$pvalue_twosided_general)
}

# Bayer and Dimitriadis (2020) tests
run_esr_backtests <- function(actual, VaR, ES, alpha, b = 0) {
  # Perform ESR backtests for all three versions
  BD_results <- lapply(1:3, function(version) {
    tryCatch(
      {
        BD <- esback::esr_backtest(r = actual, q = VaR, e = ES, alpha = alpha, version = version, B = b)
        return(BD$pvalue_twosided_asymptotic)
      },
      error = function(e) NA
    )
  })

  # Return as a named list for each version
  return(list(
    ESR1 = BD_results[[1]],
    ESR2 = BD_results[[2]],
    ESR3 = BD_results[[3]]
  ))
}

run_backtests <- function(actual, VaR, ES, alpha, prefix, VOL = NULL, b = 1000) {
  # Initialize a data frame with NA values for all expected outputs
  backtest_template <- data.frame(
    ExpectedExceedances = NA,
    ActualExceedances = NA,
    UC = NA,
    CC = NA,
    VDT = NA,
    DQ = NA,
    ER_Simple = NA,
    ER_Standardized = NA,
    ER_2 = NA,
    CoC_Simple = NA,
    CoC_Generalized = NA,
    ESR1 = NA,
    ESR2 = NA,
    ESR3 = NA
  )

  # Use tryCatch to handle errors gracefully
  safe_run <- function(expr) {
    tryCatch(
      expr,
      error = function(e) {
        message("Error encountered: ", e$message)
        return(NA)
      }
    )
  }

  # Kupiec (1995) and Christoffersen (1998) backtest
  kc_results <- safe_run(run_kc_backtest(actual = actual, VaR = VaR, alpha = alpha))
  backtest_template$ExpectedExceedances <- ifelse(is.list(kc_results), kc_results$ExpectedExceedances, NA)
  backtest_template$ActualExceedances <- ifelse(is.list(kc_results), kc_results$ActualExceedances, NA)
  backtest_template$UC <- ifelse(is.list(kc_results), kc_results$UC, NA)
  backtest_template$CC <- ifelse(is.list(kc_results), kc_results$CC, NA)

  # Christoffersen and Pelletier (2004) backtest
  vdt_pvalue <- safe_run(run_vdt_backtest(actual = actual, VaR = VaR, alpha = alpha))
  backtest_template$VDT <- vdt_pvalue

  # Dynamic Quantile (DQ) test
  dq_pvalue <- safe_run(run_dq_backtest(actual = actual, VaR = VaR, alpha = alpha))
  backtest_template$DQ <- dq_pvalue

  # McNeil and Frey (2000) backtest (simple and standardized)
  er_simple <- safe_run(run_er_backtest(actual = actual, VaR = VaR, ES = ES, vol = NULL, b = b))
  er_standardized <- if (!is.null(VOL)) {
    safe_run(run_er_backtest(actual = actual, VaR = VaR, ES = ES, vol = VOL, b = b))
  } else {
    "Not applicable"
  }
  backtest_template$ER_Simple <- er_simple
  backtest_template$ER_Standardized <- er_standardized

  # McNeil and Frey (2000) test second version
  er_pvalue_2 <- safe_run(run_er_backtest_2(actual = actual, VaR = VaR, ES = ES, alpha = alpha))
  backtest_template$ER_2 <- er_pvalue_2

  # Nolde and Ziegel (2017) backtest (simple and standardized)
  coc_simple <- safe_run(run_coc_backtest(actual = actual, VaR = VaR, ES = ES, alpha = alpha, vol = NULL))
  coc_generalized <- if (!is.null(VOL)) {
    safe_run(run_coc_backtest(actual = actual, VaR = VaR, ES = ES, alpha = alpha, vol = VOL))
  } else {
    "Not applicable"
  }
  backtest_template$CoC_Simple <- coc_simple
  backtest_template$CoC_Generalized <- coc_generalized

  # Bayer and Dimitriadis (2020) backtests
  esr_results <- safe_run(run_esr_backtests(actual = actual, VaR = VaR, ES = ES, alpha = alpha))
  backtest_template$ESR1 <- ifelse(is.list(esr_results), esr_results$ESR1, NA)
  backtest_template$ESR2 <- ifelse(is.list(esr_results), esr_results$ESR2, NA)
  backtest_template$ESR3 <- ifelse(is.list(esr_results), esr_results$ESR3, NA)

  row.names(backtest_template) <- prefix

  return(backtest_template)
}

# Negative Asymmetric Laplace Log-Likelihood (AL) Loss
custom_loss <- function(returns, VaR, ES, c) {
  loss_vector <- -log((c - 1) / ES) - ((returns - VaR) * (c - (returns <= VaR))) / (c * ES)
  return(loss_vector)
}

compute_losses_and_backtests <- function(df, models, c, n, m, b = 1000) {
  losses <- matrix(nrow = n, ncol = length(models))
  colnames(losses) <- names(models)

  backtest_template <- data.frame(
    ExpectedExceedances = NA,
    ActualExceedances = NA,
    UC = NA,
    CC = NA,
    VDT = NA,
    DQ = NA,
    ER_Simple = NA,
    ER_Standardized = NA,
    ER_2 = NA,
    CoC_Simple = NA,
    CoC_Generalized = NA,
    ESR1 = NA,
    ESR2 = NA,
    ESR3 = NA
  )

  backtest_results <- list()
  predictions <- list()

  for (model_name in names(models)) {
    print(paste("Running model:", model_name))

    # Handle errors during forecasting
    forecast <- tryCatch(
      models[[model_name]](df, c, n, m),
      error = function(e) {
        message(paste("Model", model_name, "failed:", e$message))
        return(NULL)
      }
    )

    if (!is.null(forecast)) {
      # Extract VaR and ES
      VaR <- -forecast$VaR
      ES <- -forecast$ES
      predictions[[model_name]] <- list(VaR = VaR, ES = ES)

      # Calculate losses
      losses[, model_name] <- tryCatch(
        custom_loss(tail(df$Return, n), VaR, ES, c),
        error = function(e) {
          message(paste("Loss computation failed for model", model_name, ":", e$message))
          return(rep(NA, n))
        }
      )

      # Run backtests
      backtest_results[[model_name]] <-
        run_backtests(
          actual = tail(df$Return, n),
          VaR = VaR,
          ES = ES,
          alpha = c,
          prefix = model_name,
          VOL = if ("VOL" %in% colnames(forecast)) forecast$VOL else NULL,
          b = b
        )
    } else {
      # Fill in placeholders for failed models
      losses[, model_name] <- NA
      backtest_results[[model_name]] <- backtest_template
      predictions[[model_name]] <- list(VaR = NA, ES = NA)
    }
  }

  print(backtest_results)

  # Combine all backtest results into a single dataframe
  backtest_df <- do.call(rbind, backtest_results)

  # Calculate average loss for each model
  avg_losses <- apply(losses, 2, function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE))

  # Add average losses as a column to backtest_df
  # backtest_df$AverageLoss <- avg_losses[match(rownames(backtest_df), names(avg_losses))]
  backtest_df$AverageLoss <- avg_losses[rownames(backtest_df)]

  return(list(
    losses = losses,
    backtests = backtest_df,
    predictions = predictions
  ))
}

