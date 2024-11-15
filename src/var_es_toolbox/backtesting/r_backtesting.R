library(esback)
library(rugarch)

backtest_er <- function(r, p, e, s = Null, B = 1000) {
  backtest_results <- er_backtest(r = r,
                                  p = p,
                                  e = e,
                                  s = s,
                                  B = B)
  return(backtest_results)
}

backtest_cc_uc <- function(alpha, actual, VaR, conf.level = 0.95) {
  backtest_results <- VaRTest(alpha, actual, VaR, conf.level)
  return(backtest_results)
}