import argparse
from pathlib import Path
import numpy as np

import pandas as pd

from var_es_toolbox.data import load_data

if __name__ == '__main__':
    project_dir = Path.cwd().parent
    data_dir = project_dir / "data"
    output_dir = data_dir / "refinitiv_data_merged.csv"

    data_names = ["DBc1", "DPc1", "DEBMc1", "DEPMc1", "DEBQc1", "DEPQc1", "DEBYc1", "DEPYc1"]
    data_postfix = ""
    data_col_name = "CLOSE"
    data_date_name = "Date"
    data_date_format = "%Y-%m-%d"
    fundamentals_name = "DE_Supply_and_Demand.csv"
    fundamentals_col_names = ["Total electricity supply", "Total electricity demand"]
    fundamentals_date_format = "%Y-%m-%dT%H:%M:%S"

    parser = argparse.ArgumentParser()
    parser.add_argument("--project_dir", default=project_dir, type=str)
    parser.add_argument("--data_dir", default=data_dir, type=str)
    parser.add_argument("--output_dir", default=output_dir, type=str)
    parser.add_argument("--data_names", default=data_names, type=int, nargs='+', help="List of name of files to merge")
    parser.add_argument("--data_postfix", default=data_postfix, type=str)
    parser.add_argument("--data_col_name", default=data_col_name, type=str)
    parser.add_argument("--data_date_name", default=data_date_name, type=str)
    parser.add_argument("--data_date_format", default=data_date_format, type=str)
    parser.add_argument("--fundamentals_name", default=fundamentals_name, type=str)
    parser.add_argument("--fundamentals_col_names", default=fundamentals_col_names, type=int, nargs='+', help="List of indices for fundamentals")
    parser.add_argument("--fundamentals_date_format", default=fundamentals_date_format, type=str)
    args = parser.parse_args()

    project_dir = args.project_dir
    data_dir = args.data_dir
    output_dir = args.output_dir
    data_names = args.data_names
    data_postfix = args.data_postfix
    data_col_name = args.data_col_name
    data_date_name = args.data_date_name
    data_date_format = args.data_date_format
    fundamentals_name = args.fundamentals_name
    fundamentals_col_names = args.fundamentals_col_names
    fundamentals_date_format = args.fundamentals_date_format

    data_futures = pd.DataFrame()
    for data_name in data_names:
        data_path = data_dir / f"{data_name}{data_postfix}.csv"
        data_future = load_data(data_path, date_format=data_date_format, index_col=data_date_name)
        data_future = data_future[[data_col_name]].rename(columns={data_col_name: data_name})
        data_future[f"{data_name}_return"] = np.log(data_future[data_name].pct_change() + 1)

        if data_futures.empty:
            data_futures = data_future
        else:
            data_futures = pd.merge(data_futures, data_future, left_index=True, right_index=True, how="outer", sort=True)

    data_fundamentals = load_data(data_dir / fundamentals_name, date_format=fundamentals_date_format, index_col=data_date_name)
    data_fundamentals = data_fundamentals[fundamentals_col_names]

    data_merged = pd.merge(data_futures, data_fundamentals, left_index=True, right_index=True, how="outer", sort=True)

    print(data_merged.index)
    data_merged.to_csv(output_dir, index=True)
