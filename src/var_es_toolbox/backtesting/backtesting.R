library(esback)
library(rugarch)

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
    BD <- esback::esr_backtest(r = actual, q = VaR, e = ES, alpha = alpha, version = version, B = b)
    return(BD$pvalue_twosided_asymptotic)
  })

  # Return as a named list for each version
  return(list(
    ESR1 = BD_results[[1]],
    ESR2 = BD_results[[2]],
    ESR3 = BD_results[[3]]
  ))
}

run_backtests <- function(actual, VaR, ES, alpha, prefix, VOL = NULL, b = 1000) {

  # Kupiec (1995) and Christoffersen (1998) backtest
  kc_results <- run_kc_backtest(actual = actual, VaR = VaR, alpha = alpha)
  KC_df <- data.frame(
    "ExpectedExceedances" = kc_results$ExpectedExceedances,
    "ActualExceedances" = kc_results$ActualExceedances,
    "UC" = kc_results$UC,
    "CC" = kc_results$CC
  )

  # McNeil and Frey (2000) backtest
  er_pvalue <- run_er_backtest(actual = actual, VaR = VaR, ES = ES, vol = VOL, b = b)
  ER_df <- data.frame(
    "ER" = er_pvalue
  )

  # McNeil and Frey (2000) test second version
  er_pvalue <- run_er_backtest_2(actual = actual, VaR = VaR, ES = ES, alpha = alpha)
  ER_df_2 <- data.frame(
    "ER_2" = er_pvalue
  )

  # Nolde and Ziegel (2017) backtest
  coc_pvalue <- run_coc_backtest(actual = actual, VaR = VaR, ES = ES, vol = VOL, alpha = alpha)
  COC_df <- data.frame(
    "CoC" = coc_pvalue
  )

  # Bayer and Dimitriadis (2020) backtests
  esr_results <- run_esr_backtests(actual = actual, VaR = VaR, ES = ES, alpha = alpha)
  ESR_df <- data.frame(
    "ESR1" = esr_results$ESR1,
    "ESR2" = esr_results$ESR2,
    "ESR3" = esr_results$ESR3
  )

  # Combine all results into a single dataframe
  results <- cbind(KC_df, ER_df, ER_df_2, COC_df, ESR_df)
  row.names(results) <- prefix

  return(results)
}
