from pathlib import Path
from renv_utils import activate_renv, snapshot_renv
from var_es_toolbox.data import load_data

activate_renv()
# snapshot_renv()

from rpy2.robjects import globalenv, r

project_dir = Path(__file__).resolve().parents[1]

if __name__ == '__main__':
    # # Retrieve data
    # data_dir = project_dir / "data"
    # data_cleaned_name = "data_cleaned.csv"
    # date_format = "%Y-%m-%d"
    # data_cleaned = load_data(data_dir / data_cleaned_name, date_format=date_format)
    # futures_returns = data_cleaned.iloc[:, 1]
    #
    # # Retrieve r model
    # models_dir = project_dir / "src" / "var_es_toolbox" / "models"
    # r_models_path = models_dir / "r_arch_models.R"
    # r.source(str(r_models_path))
    # forecast_ugarch_r = r["forecast_ugarch"]
    # forecast_ugarch_result_r = forecast_ugarch_r(futures_returns, 1, 1, 5)
    # print(forecast_ugarch_result_r)

    r('''renv::install("quarks")''')

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
