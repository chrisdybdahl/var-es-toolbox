import argparse
from pathlib import Path

from var_es_toolbox.data import load_data

if __name__ == '__main__':
    project_dir = Path.cwd().parent
    data_dir = project_dir / "data"
    output_dir = data_dir / "data_cleaned.csv"
    data_merged_name = "data_merged.csv"
    date_format = "%Y-%m-%d"
    future_n = [0]

    parser = argparse.ArgumentParser()
    parser.add_argument("--project_dir", default=project_dir, type=str)
    parser.add_argument("--data_dir", default=data_dir, type=str)
    parser.add_argument("--output_dir", default=output_dir, type=str)
    parser.add_argument("--data_merged_name", default=data_merged_name, type=str)
    parser.add_argument("--date_format", default=date_format, type=str)
    parser.add_argument("--future_n", default=future_n, type=int, nargs='+', help="List of indices for futures")
    args = parser.parse_args()

    project_dir = args.project_dir
    data_dir = args.data_dir
    output_dir = args.output_dir
    data_merged_name = args.data_merged_name
    date_format = args.date_format
    future_n = args.future_n

    data_merged = load_data(data_dir / data_merged_name, date_format=date_format)
    data_merged = data_merged[(data_merged.iloc[:, 1] >= -100) & (data_merged.iloc[:, 1] <= 100)]

    data_merged.to_csv(output_dir, index=True)
