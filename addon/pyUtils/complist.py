"""Module for calculating Mie potential."""
from __future__ import print_function
import numpy as np
from sys import exit
from compdata import comp_list
import os


compl = comp_list()
compl.save_fortran_file("compdatadb.f90")
