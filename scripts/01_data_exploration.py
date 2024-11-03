import argparse
from pathlib import Path

import pandas as pd

from var_es_toolbox.data import load_data

if __name__ == '__main__':
    project_dir = Path.cwd().parent
    data_dir = project_dir / "data"
    output_dir = data_dir / "data_merged.csv"
    data_futures_name = "DE_NPE_Daily_Base.csv"
    data_fundamentals_name = "DE_Supply_and_Demand.csv"
    date_format = "%d-%m-%y %H:%M"
    future_n = [0]
    fund_n = [-1, -2]

    parser = argparse.ArgumentParser()
    parser.add_argument("--project_dir", default=project_dir, type=str)
    parser.add_argument("--data_dir", default=data_dir, type=str)
    parser.add_argument("--output_dir", default=output_dir, type=str)
    parser.add_argument("--data_futures_name", default=data_futures_name, type=str)
    parser.add_argument("--data_fundamentals_name", default=data_fundamentals_name, type=str)
    parser.add_argument("--date_format", default=date_format, type=str)
    parser.add_argument("--future_n", default=future_n, type=int, nargs='+', help="List of indices for futures")
    parser.add_argument("--fund_n", default=fund_n, type=int, nargs='+', help="List of indices for fundamentals")
    args = parser.parse_args()

    project_dir = args.project_dir
    data_dir = args.data_dir
    output_dir = args.output_dir
    data_futures_name = args.data_futures_name
    data_fundamentals_name = args.data_fundamentals_name
    date_format = args.date_format
    future_n = args.future_n
    fund_n = args.fund_n

    data_futures = load_data(data_dir / data_futures_name, date_format=date_format)
    future_names = data_futures.columns[future_n]
    futures = data_futures[future_names].copy()
    for future_name in future_names:
        futures[f"{future_name} - return"] = futures[future_name].pct_change() * 100

    data_fundamentals = load_data(data_dir / data_fundamentals_name, date_format=date_format)
    fundamentals_name = data_fundamentals.columns[fund_n]
    fundamentals = data_fundamentals[fundamentals_name]

    data_merged = pd.merge(futures, fundamentals, left_index=True, right_index=True).dropna()
    data_merged.to_csv(output_dir, index=True)
