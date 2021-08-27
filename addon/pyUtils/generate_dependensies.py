from __future__ import print_function
import numpy as np
from sys import exit
import math
import os

N_FILE_PER_LINE = 2

def add_to_list_if_new(dep_list, new_dep):
    """
    Add new_dep to dep_list if not already present
    """
    new_dep_in_list = False
    for dep in dep_list:
        if dep == new_dep:
            new_dep_in_list = True
            exit
    if not new_dep_in_list:
        dep_list.append(new_dep)
    return dep_list

def get_file_dependencies(filepath, module_list):
    """
    Parse file (filepath) for use statements, and add
    all included modules to dependencies list
    """
    dependensies = []
    with open(filepath, encoding="ISO-8859-1", mode="r") as f:
        for line in f:
            word_array = line.replace(",", " ").split()
            if len(word_array) > 0:
                if word_array[0].lower() == "use":
                    if word_array[1].lower() in module_list:
                        add_to_list_if_new(dependensies, word_array[1].lower())
    return dependensies

def build_make_string(filepath, deps):
    """
    Make list containg build nformation for file (filepath)
    with dependencies (deps)
    """
    filename_src = filepath.split("/")[-1]
    fend = filepath.split(".")[-1]
    filename_o = filename_src.replace(fend, "o")
    lines = []
    # Only process FORTRAN files
    if fend == "f90":
        if len(deps) == 0:
            line_end = ""
        else:
            line_end = " \\"
        lines.append("$(ODIR)/" + filename_o + ": $(SRC)/" + filename_src + line_end)

        # Loop dependencies
        nMax = len(deps)
        for i in range(math.ceil(nMax/N_FILE_PER_LINE)):
            line = "\t"
            jmax = min(nMax-i*N_FILE_PER_LINE, N_FILE_PER_LINE)
            for j in range(jmax):
                jj = i*N_FILE_PER_LINE+j
                if j == jmax-1:
                    if jj == nMax-1:
                        line += "$(SRC)/" + deps[jj] + ".f90"
                    else:
                        line += "$(SRC)/" + deps[jj] + ".f90 \\"
                else:
                    line += "$(SRC)/" + deps[jj] + ".f90 "
            lines.append(line)
        lines.append("\t" + "@$(call make_object, $(SRC)/" + filename_src + ")")
    return lines

def save_make_lines(filename, make_lines):
    """ Save entries of make_lines as lines in filename
    """
    with open(filename, mode="w") as f:
        for line in make_lines:
            f.write(line+"\n")


if __name__ == "__main__":
    # Source files
    file_list = os.listdir("../../src/")
    file_list.sort()
    # Possible module names
    module_list = []
    for f in file_list:
        mod_name = f.split(".")[0]
        module_list.append(mod_name)


    # Generate make_file to be copied to Makefile.code
    make_lines = []
    for fl in file_list:
        filepath = os.path.join("../../src/", fl)
        deps = get_file_dependencies(filepath, module_list)
        lines = build_make_string(filepath, deps)
        for line in lines:
            make_lines.append(line)
        print(fl, ": ", deps)
        print(lines)
    save_make_lines("make_file", make_lines)
