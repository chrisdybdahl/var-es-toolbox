import os
from pathlib import Path

import rpy2.situation as rsetup

project_dir = Path(__file__).resolve().parents[1]
os.environ["HOME"] = str(project_dir)
os.environ["R_HOME"] = rsetup.get_r_home()
from rpy2.robjects import pandas2ri, r, globalenv


def init_renv(show_diagnose=False):
    globalenv["project_dir"] = str(project_dir)
    r('''
    renv::init(project = project_dir)
    lib_dir <- renv::paths$library(project = project_dir)
    .libPaths(lib_dir)
    ''')
    if show_diagnose:
        r('''
        renv::diagnostics(project = project_dir)
        ''')
    pandas2ri.activate()


def activate_renv(show_diagnose=False):
    globalenv["project_dir"] = str(project_dir)
    r('''
    renv::activate(project = project_dir)
    lib_dir <- renv::paths$library(project = project_dir)
    .libPaths(lib_dir)
    ''')
    if show_diagnose:
        r('''
        renv::diagnostics(project = project_dir)
        ''')
    pandas2ri.activate()


def snapshot_renv(show_diagnose=False):
    globalenv["project_dir"] = str(project_dir)
    r('''
    renv::snapshot(project = project_dir)
    ''')
    if show_diagnose:
        r('''
        renv::diagnostics(project = project_dir)
        ''')
