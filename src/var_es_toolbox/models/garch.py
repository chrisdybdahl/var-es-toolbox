from arch import arch_model
from arch.univariate.base import ARCHModelForecast
from pandas import DataFrame


def forecast_ugarch(data: DataFrame, p: int = 1, q: int = 1, n: int = 10, verbose: int = 0, **kwargs) -> ARCHModelForecast:
    garch_model = arch_model(data, vol="GARCH", p=p, q=q, **kwargs)

    if verbose == 1:
        garch_result = garch_model.fit()
        print(garch_result.summary())
    else:
        garch_result = garch_model.fit(update_freq=0, disp='off')

    forecast = garch_result.forecast(horizon=n)
    return forecast
