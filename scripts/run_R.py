from pathlib import Path
from renv_utils import activate_renv, snapshot_renv

activate_renv()
# snapshot_renv()

from rpy2.robjects import globalenv, r, pandas2ri

project_dir = Path(__file__).resolve().parents[1]

if __name__ == '__main__':
    r('''
    renv::install("C:/Users/chris/RStudioProjects/caviar")
    # renv::remove("caviar")
    # library(Rcpp)
    # cppFunction("int add(int x, int y) { return x + y; }")
    # x <- add(1, 2)  # Should return 3
    # print(x)
    # Rcpp::compileAttributes(pkgdir = "C:/Users/chris/PycharmProjects/var-es-toolbox/src/var_es_toolbox/models/caviar")
    ''')
