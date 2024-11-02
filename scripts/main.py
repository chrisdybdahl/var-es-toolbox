import os
from pathlib import Path

import pandas as pd
import rpy2.situation as rsetup

from var_es_toolbox.data import load_data

os.environ["R_HOME"] = rsetup.get_r_home()
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri

if __name__ == '__main__':
    print(os.environ["R_HOME"])
    print(rsetup)
    project_dir = Path.cwd().parent
    data_dir = project_dir / "data"
    data_futures_name = "DE_NPE_Daily_Base.csv"
    data_fundamentals_name = "DE_Supply_and_Demand.csv"

    data_futures = load_data(data_dir / data_futures_name)
    data_fundamentals = load_data(data_dir / data_fundamentals_name)

    data = pd.merge(data_futures, data_fundamentals, left_index=True, right_index=True).dropna()

    pandas2ri.activate()
    robjects.globalenv['df'] = pandas2ri.py2rpy(data)

    robjects.r('''
    library(ggplot2)
    ggplot(df, aes(x=`Total electricity supply`, y=`Total electricity demand`)) +
    geom_point(color="blue") +
    ggtitle("Electricity Supply vs Demand") +
    xlab("Total electricity supply") +
    ylab("Total electricity demand")''')

    # robjects.r('''
    #            renv::init(project = "C:/Users/chris/PycharmProjects/var-es-toolbox")
    #        ''')

    """robjects.r('''
            project_dir <- rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::has_file("renv.lock"))
            renv::activate(project = project_dir)
        ''')"""

    """pandas2ri.activate()
    robjects.globalenv['df'] = pandas2ri.py2rpy(data)
    robjects.r('''
        plot(df$x, df$y, main="Plot of X vs Y", xlab="X Axis", ylab="Y Axis")
    ''')"""
