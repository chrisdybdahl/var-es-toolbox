from pathlib import Path

import pandas as pd


def load_data(data_dir: Path, date_format: str = None) -> pd.DataFrame:
    data = pd.read_csv(data_dir, sep=",", index_col=0)
    if date_format:
        data.index = pd.to_datetime(data.index, format=date_format)
    else:
        data.index = pd.to_datetime(data.index, dayfirst=True)

    return data
