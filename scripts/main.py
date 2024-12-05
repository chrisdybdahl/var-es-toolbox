from pathlib import Path
from renv_utils import activate_renv, snapshot_renv
from var_es_toolbox.data import load_data

activate_renv()
# snapshot_renv()

from rpy2.robjects import globalenv, r, pandas2ri

project_dir = Path(__file__).resolve().parents[1]

# TODO: In all models, implement that ... send in args to underlying function

if __name__ == '__main__':
    # Asset
    asset = "DEBQc1_return"
    globalenv["asset"] = str(asset)

    # Retrieve data
    data_dir = project_dir / "data"
    data_cleaned_name = "refinitiv_data_merged.csv"
    date_format = "ISO8601"
    data_cleaned = load_data(data_dir / data_cleaned_name, date_format=date_format)
    futures_returns = data_cleaned[asset].dropna()

    # Retrieve r model
    models_dir = project_dir / "src" / "var_es_toolbox" / "models"
    backtest_dir = project_dir / "src" / "var_es_toolbox" / "backtesting"
    hs_path = models_dir / "hs.R"
    garch_path = models_dir / "garch.R"
    gas_path = models_dir / "gas.R"
    hybrid_evt_path = models_dir / "hybrid_evt.R"
    caviar_path = models_dir / "caviar" / "caviar.R"
    backtest_path = backtest_dir / "backtesting.R"

    globalenv['hs_path'] = str(hs_path)
    globalenv['garch_path'] = str(garch_path)
    globalenv['gas_path'] = str(gas_path)
    globalenv['hybrid_evt_path'] = str(hybrid_evt_path)
    globalenv['caviar_path'] = str(caviar_path)
    globalenv['backtest_path'] = str(backtest_path)

    # r.source(str(r_arch_models_path))
    # r.source(str(r_hybrid_evt_models_path))
    # r.source(str(r_non_param_path))
    # r.source(str(r_gas_models_path))
    # r.source(str(r_caviar_models_path))

    c = 0.05
    t = 0.95
    p = 1
    q = 1
    m = 500
    n = 200
    refit = 10

    globalenv['df'] = pandas2ri.py2rpy(futures_returns.reset_index())
    globalenv['df_all'] = pandas2ri.py2rpy(data_cleaned.reset_index())
    globalenv['c'] = c
    globalenv['p'] = p
    globalenv['q'] = q
    globalenv['m'] = m
    globalenv['n'] = n
    globalenv['refit'] = refit

    # Initialize in R
    r('''
    library(xts)
    library(rugarch)
    library(psych)
    library(ggplot2)             
    
    names(df)[names(df) == asset] <- "Return"
    dates <- tail(df$Date, n)
    returns <- xts(tail(df$Return, n), order.by = dates)
    
    desc_stat <- psych::describe(df_all)
    print(desc_stat)
    
    # Define the function for plotting returns, VaR, and ES
    plot_var_es <- function(dates, returns, var, es) {
      # Create a data frame for plotting
      plot_data <- data.frame(
        Date = dates,
        Return = returns,
        VaR = var,
        ES = es
      )
      
      # Generate the plot using ggplot2 with labels
      ggplot(plot_data, aes(x = Date)) +
        geom_line(aes(y = Return, color = "Return"), linewidth = 0.7, alpha = 0.8) +    # Actual returns
        geom_line(aes(y = VaR, color = "VaR"), linetype = "dashed", linewidth = 0.7) + # VaR
        geom_line(aes(y = ES, color = "ES"), linetype = "dotted", linewidth = 0.7) +   # ES
        ggtitle("Actual Returns vs. VaR and ES") +
        xlab("Date") +
        ylab("Returns / VaR / ES") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
        ) +
        scale_color_manual(values = c("Return" = "blue", "VaR" = "red", "ES" = "darkorange")) +
        scale_y_continuous(labels = scales::percent) +
        labs(color = "Legend")
    }
    ''')

    # Run VaR forecast
    r('''
    source(caviar_path)
    source(backtest_path)

    result <- forecast_u_CAViaR(df, c, n, m, r = 100, var_model = "ADAPTIVE", es_model="AR")
    
    var <- -result$VaR
    VaRplot(c, returns, var)

    es <- -result$ES
    print(plot_var_es(dates, returns, var, es))

    vol <- result$VOL
    run_backtests(returns, var, es, c, prefix = "CAVIAR")
    ''')

    # r('''renv::install("evir")''')

    # r('''
    # ggplot(df, aes(x=`Date`, y=`NPE EDEFB D1 - return`)) +
    # geom_point(color="blue") +
    # ggtitle("NPE EDEFB D1 Returns") +
    # xlab("Date") +
    # ylab("Return (%)")
    # ''')
    #
    # # Retrieve backtest model
    # backtesting_dir = project_dir / "src" / "var_es_toolbox" / "backtesting"
    # r_backtesting_path = backtesting_dir / "backtesting.R"
    # r.source(str(r_backtesting_path))
    # backtest_er = r["backtest_er"]
    # backtest_er_result = backtest_er([0.1], [5], [4.5], [30], 1000)
    # print(backtest_er_result)
