from pathlib import Path
from renv_utils import activate_renv, snapshot_renv
from var_es_toolbox.data import load_data

activate_renv()
snapshot_renv()

from rpy2.robjects import globalenv, r, pandas2ri

project_dir = Path(__file__).resolve().parents[1]

if __name__ == '__main__':
    # Retrieve data
    data_dir = project_dir / "data"
    data_cleaned_name = "refinitiv_data_cleaned.csv"
    date_format = "ISO8601"
    data_cleaned = load_data(data_dir / data_cleaned_name, date_format=date_format)
    futures_returns = data_cleaned.iloc[:, 1]

    # Retrieve r model
    models_dir = project_dir / "src" / "var_es_toolbox" / "models"
    r_arch_models_path = models_dir / "r_arch_models.R"
    r_hybrid_evt_models_path = models_dir / "r_hybrid_evt_models.R"
    r_non_param_path = models_dir / "non_parametric_models.R"
    r_gas_models_path = models_dir / "r_gas_models.R"
    r_caviar_models_path = models_dir / "r_caviar_models.R"

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
    library(rugarch)
    library(xts)
    library(psych)
    library(ggplot2)             
    library(parallel)
    
    names(df)[names(df) == "DBc1_return"] <- "Return"
    returns <- xts(tail(df$Return, n), order.by = tail(df$Date, n))
    
    desc_stat <- psych::describe(df_all)
    print(desc_stat)
    
    ggplot(df, aes(x=Date, y=Return)) +
        geom_line(color="blue") +
        ggtitle("DBc1 Returns") +
        xlab("Date") +
        ylab("Return (%)")
    ''')

    # Run VaR forecast
    r('''
    source(r_hybrid_evt_models_path)
    
    var <- forecast_u_EVT_GARCH_var(df, c, n, m, t = 0.95)
    print(var)
    VaRplot(c, returns, var)
    print(VaRTest(c, returns, var))
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
    # r_backtesting_path = backtesting_dir / "r_backtesting.R"
    # r.source(str(r_backtesting_path))
    # backtest_er = r["backtest_er"]
    # backtest_er_result = backtest_er([0.1], [5], [4.5], [30], 1000)
    # print(backtest_er_result)
