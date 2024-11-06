import os

from pathlib import Path

# import rpy2.situation as rsetup
# os.environ["R_HOME"] = rsetup.get_r_home()

os.environ["R_HOME"] = "C:/Users/chris/Anaconda/envs/var-es-r/lib/R"
import rpy2.robjects as robjects


def activate_renv():
    robjects.globalenv["project_dir"] = str(Path(__file__).resolve().parents[2])
    robjects.r('''
        renv::activate(project = project_dir)
        lib_dir <- renv::paths$library(project = project_dir)
        .libPaths(lib_dir)
        ''')


if __name__ == '__main__':
    activate_renv()
