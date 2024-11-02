from arch import arch_model
from arch.univariate import HARX


def garch(data, p=1, q=1, **kwargs) -> HARX:
    return arch_model(data, vol="GARCH", p=p, q=q, **kwargs)
