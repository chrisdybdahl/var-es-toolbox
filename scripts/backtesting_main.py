import os
from pathlib import Path

from var_es_toolbox.data import load_data

os.environ["R_HOME"] = "C:/Users/chris/Anaconda/envs/var-es-r/lib/R"

import rpy2.robjects as robjects

if __name__ == '__main__':
    project_dir = Path(__file__).resolve().parents[1]
    robjects.globalenv["project_dir"] = str(project_dir)
    robjects.r('''
    renv::activate(project = project_dir)
    lib_dir <- renv::paths$library(project = project_dir)
    .libPaths(lib_dir)
    print(.libPaths())
    print(find.package("stats"))
    renv::diagnostics(project = project_dir)
    library(stats, lib.loc = "var-es-r")
    print("done")
    ''')

    # Retrieve data
    data_dir = project_dir / "data"
    data_cleaned_name = "data_cleaned.csv"
    date_format = "%Y-%m-%d"
    data_cleaned = load_data(data_dir / data_cleaned_name, date_format=date_format)
    futures_returns = data_cleaned.iloc[:, 1]

    # Retrieve r model
    models_dir = project_dir / "src" / "var_es_toolbox" / "models"
    r_models_path = models_dir / "r_models.R"
    robjects.globalenv['r_models_path'] = str(r_models_path)
    robjects.r.source(str(r_models_path))
    forecast_ugarch_r = robjects.r["forecast_ugarch"]
    forecast_ugarch_result_r = forecast_ugarch_r(futures_returns, 1, 1, 5)

    exit()

    backtesting_dir = project_dir / "src" / "var_es_toolbox" / "backtesting"
    r_backtesting_path = backtesting_dir / "r_backtesting.R"
    robjects.globalenv['r_backtesting_path'] = str(r_backtesting_path)
    # robjects.r.source(str(r_models_path))
    robjects.r('''
    source(r_backtesting_path)
    ''')

    backtest_er = robjects.r["backtest_er"]
    backtest_er_result = backtest_er([0.1], [5], [4.5], [30], 1000)
    print(backtest_er_result)


    robjects.r('''
    lib_dir <- renv::paths$library(project = project_dir)
    .libPaths(lib_dir)
    renv::install("esback")
    renv::install("rugarch")
    renv::install("ggplot2")
    ''')