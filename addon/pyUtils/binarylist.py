"""Module for calculating Mie potential."""
from __future__ import print_function
import numpy as np
from sys import exit
from binarydata import binary_list
import os


binl = binary_list()
binl.save_fortran_file("mixdatadb.f90")
