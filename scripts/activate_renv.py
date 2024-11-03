import os

import rpy2.situation as rsetup

os.environ["R_HOME"] = rsetup.get_r_home()
import rpy2.robjects as robjects


def activate_renv():
    robjects.r('''
        project_dir <- rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::has_file("renv.lock"))
        renv::activate(project = project_dir)
        ''')
