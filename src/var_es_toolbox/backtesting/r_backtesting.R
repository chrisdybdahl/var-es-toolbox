library(esback)

backtest_er <- function(r, p, e, s = Null, B = 1000) {
  backtest_results <- er_backtest(r = r,
                                  p = p,
                                  e = e,
                                  s = s,
                                  B = B)
  return(backtest_results)
}