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
    asset = "DPc1_return"
    globalenv["asset"] = str(asset)

    # Retrieve data
    data_dir = project_dir / "data"
    data_cleaned_name = "refinitiv_data_merged.csv"
    date_format = "ISO8601"
    data_cleaned = load_data(data_dir / data_cleaned_name, date_format=date_format)
    futures_returns = data_cleaned[asset].dropna()

    # Retrieve r model
    models_dir = project_dir / "src" / "var_es_toolbox" / "models"
    r_arch_models_path = models_dir / "garch.R"
    r_hybrid_evt_models_path = models_dir / "hybrid_evt.R"
    r_non_param_path = models_dir / "hs.R"
    r_gas_models_path = models_dir / "gas.R"
    r_caviar_models_path = models_dir / "caviar.R"

    globalenv['r_non_param_path'] = str(r_non_param_path)
    globalenv['r_arch_models_path'] = str(r_arch_models_path)
    globalenv['r_hybrid_evt_models_path'] = str(r_hybrid_evt_models_path)
    globalenv['r_gas_models_path'] = str(r_gas_models_path)
    globalenv['r_caviar_models_path'] = str(r_caviar_models_path)

    # r.source(str(r_arch_models_path))
    # r.source(str(r_hybrid_evt_models_path))
    # r.source(str(r_non_param_path))
    # r.source(str(r_gas_models_path))
    # r.source(str(r_caviar_models_path))

    c = 0.05
    t = 0.95
    p = 1
    q = 1
    m = 1000
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
    library(psych)
    library(ggplot2)
    
    names(df)[names(df) == asset] <- "Return"
    dates <- tail(df$Date, n)
    returns <- xts(tail(df$Return, n), order.by = tail(df$Date, n))
    
    desc_stat <- psych::describe(df_all)
    print(desc_stat)
    
    gg <- ggplot(df, aes(x=Date, y=Return)) +
        geom_line(color="blue") +
        ggtitle("DBc1 Returns") +
        xlab("Date") +
        ylab("Return (%)")
    print(gg)
           
    # Define the function for plotting returns, VaR, and ES
    plot_var_es <- function(dates, returns, var, es) {
      # Create a data frame for plotting
      plot_data <- data.frame(
        Date = dates,
        Return = returns,
        VaR = var,
        ES = es
      )
      
      # Generate the plot using ggplot2
      ggplot(plot_data, aes(x = Date)) +
        geom_line(aes(y = Return), color = "blue", linewidth = 0.7, alpha = 0.8) +    # Actual returns
        geom_line(aes(y = VaR), color = "red", linetype = "dashed", linewidth = 0.7) + # VaR
        geom_line(aes(y = ES), color = "darkorange", linetype = "dotted", linewidth = 0.7) + # ES
        ggtitle("Actual Returns vs. VaR and ES") +
        xlab("Date") +
        ylab("Returns / VaR / ES") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "top"
        ) +
        scale_y_continuous(labels = scales::percent)
    }
    Sys.sleep(10)
    ''')

    # Run VaR forecast
    r('''
    source(r_arch_models_path)

    result <- forecast_u_GARCH(df, c, n, m)
    
    var <- -result$VaR
    VaRplot(c, returns, var)
    print(VaRTest(c, returns, var))
    
    es <- -result$ES
    print(ESTest(c, returns, es, var))
    
    plot_var_es(dates, returns, var, es)
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
