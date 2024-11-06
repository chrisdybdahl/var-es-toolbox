import os

from pathlib import Path

# import rpy2.situation as rsetup
# os.environ["R_HOME"] = rsetup.get_r_home()

os.environ["R_HOME"] = "C:/Users/chris/Anaconda/envs/var-es-r/lib/R"
import rpy2.robjects as robjects

if __name__ == '__main__':
    project_dir = Path(__file__).resolve().parents[1]
    robjects.globalenv["project_dir"] = str(project_dir)
    print(project_dir)

    robjects.r('''
    renv::activate(project = project_dir)
    ''')

    # lib_dir = project_dir / "renv" / "library" / "windows" / "R-4.4" / "x86_64-w64-mingw32"
    # robjects.globalenv["lib_dir"] = str(lib_dir)

    robjects.r('''
    lib_dir <- renv::paths$library(project = project_dir)
    .libPaths(lib_dir)
    print(lib_dir)
    print(renv::paths)
    ''')

    """robjects.r('''
    renv::snapshot(project = project_dir)
    ''')"""

    robjects.r('''
    renv::install("esback")
    renv::install("rugarch")
    renv::install("ggplot2")
    ''')
