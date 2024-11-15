from pathlib import Path

import pandas as pd


def load_data(data_dir: Path, date_format: str = None, index_col: int | str = 0) -> pd.DataFrame:
    data = pd.read_csv(data_dir, sep=",", index_col=index_col)
    if date_format:
        try:
            data.index = pd.to_datetime(data.index, format=date_format)
        except Exception as e:
            print(f"Error occurred during date parsing {e}\nTrying to infer datetime format")
            data.index = pd.to_datetime(data.index, format="mixed")
    else:
        data.index = pd.to_datetime(data.index, format="mixed")

    return data
