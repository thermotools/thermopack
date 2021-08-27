"""Module manipulating binary interaction data in thermopack"""
from __future__ import print_function
import numpy as np
from sys import exit
import os
import math
import json
from datetime import datetime

I = "  "
N_TAGS_PER_LINE = 5

class binaries(object):
    """Read binary data form file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        self.eos = os.path.basename(filepath).split("_")[0]
        if "kij" in os.path.basename(filepath) or "lij" in os.path.basename(filepath):
            self.mixing = "vdW"
        else:
            self.mixing = "GE"
        self.filepath = filepath
        with open(filepath) as f:
            self.bins = json.load(f)

    def save(self):
        """Save to file
        """
        with open(self.filepath, "w") as f:
            json.dump(self.bins,f,indent=2)

    def get_vdW_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type (kijdatadb), parameter :: VDWTAG" + " = &")
        code_lines.append(3*I+"kijdatadb(eosid = \"" + self.eos + "\", &")
        code_lines.append(3*I+"mruleid = \"vdW\", &")
        code_lines.append(3*I+"ref = \"" + self.bins[tag]["ref"] + "\", &")
        code_lines.append(3*I+"bib_ref = \"" + self.bins[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I+"uid1 = \"" + self.bins[tag]["comp1"] + "\", &")
        code_lines.append(3*I+"uid2 = \"" + self.bins[tag]["comp2"] + "\", &")
        code_lines.append(3*I + 'kijvalue = {:.8f}'.format(self.bins[tag]["kij"]) + "  &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_lij_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type (lijdatadb), parameter :: LIJTAG" + " = &")
        code_lines.append(3*I+"lijdatadb(eosid = \"" + self.eos + "\", &")
        code_lines.append(3*I+"mruleid = \"vdW\", &")
        code_lines.append(3*I+"ref = \"" + self.bins[tag]["ref"] + "\", &")
        code_lines.append(3*I+"bib_ref = \"" + self.bins[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I+"uid1 = \"" + self.bins[tag]["comp1"] + "\", &")
        code_lines.append(3*I+"uid2 = \"" + self.bins[tag]["comp2"] + "\", &")
        code_lines.append(3*I + 'lijvalue = {:.8f}'.format(self.bins[tag]["lij"]) + "  &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_GE_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """
        mixing = tag.split("-")[0]
        code_lines = []
        code_lines.append(I + "type (interGEdatadb), parameter :: GETAG" + " = &")
        code_lines.append(3*I + "interGEdatadb(eosid = \"" + self.eos + "\", &")
        code_lines.append(3*I + "mruleid = \"" + mixing + "\", &")
        code_lines.append(3*I + "ref = \"" + self.bins[tag]["ref"] + "\", &")
        code_lines.append(3*I + "bib_ref = \"" + self.bins[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I + "uid1 = \"" + self.bins[tag]["comp1"] + "\", &")
        code_lines.append(3*I + "uid2 = \"" + self.bins[tag]["comp2"] + "\", &")
        code_lines.append(3*I + 'kijvalue = {:.8f}'.format(self.bins[tag]["kij"]) + ", &")
        correlation = "1"
        assert self.bins[tag]["correlation"] == "Default"
        code_lines.append(3*I + "correlation = " + correlation + ", &")
        code_lines.append(3*I + 'alphaijvalue = (/{:.8e}, {:.8e}/)'.format(self.bins[tag]["alphaij"], self.bins[tag]["alphaji"]) + ", &")
        ge = []
        for numbers in self.bins[tag]["polyij"]:
            gei = '{:.8e}'.format(numbers)
            ge.append(gei)
        if len(ge) == 2:
            ge.append("0.0")
        code_lines.append(3*I + "polyij = (/" + ge[0] + ", " + ge[1] + ", " + ge[2] + "/), &")
        ge = []
        for numbers in self.bins[tag]["polyji"]:
            gei = '{:.8e}'.format(numbers)
            ge.append(gei)
        if len(ge) == 2:
            ge.append("0.0")
        code_lines.append(3*I + "polyji = (/" + ge[0] + ", " + ge[1] + ", " + ge[2] + "/) &")
        code_lines.append(3*I + ")")
        code_lines.append("")
        return code_lines

    def get_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """
        code_lines_vdW = []
        code_lines_GE = []
        code_lines_lij = []
        for key in self.bins:
            if "vdW" in key or "LK" in key:
                cl = self.get_vdW_fortran_code(key)
                for line in cl:
                    code_lines_vdW.append(line)
            elif "LIJ" in key:
                cl = self.get_lij_fortran_code(key)
                for line in cl:
                    code_lines_lij.append(line)
            else:
                cl = self.get_GE_fortran_code(key)
                for line in cl:
                    code_lines_GE.append(line)
        return code_lines_vdW, code_lines_GE, code_lines_lij

class binary_list(object):
    """Read binary data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None):
        """Load files from data-base
        Arguments:
        filename - path to folder
        """
        # Define counters
        self.nVDW = None
        self.nGE = None
        if path is None:
            path = "../../binaries/"
        file_list = os.listdir(path)
        file_list.sort()
        self.bin_list = []
        for fl in file_list:
            filepath = os.path.join(path, fl)
            print(fl)
            self.bin_list.append(binaries(filepath))

    def save_fortran_file(self,filename):
        """Generate fortran file for Thermopack
        Arguments:
        filename - path to folder
        """
        header = []
        header.append("!> Automatically generated to file mixdatadb.f90")
        header.append("!! using utility python code pyUtils")
        now = datetime.today().isoformat()
        header.append("!! Time stamp: " + now)
        header.append("")
        header.append("module mixdatadb")
        header.append(I+"use cubic_eos, only: kijdatadb, interGEdatadb, lijdatadb")
        header.append(I+"implicit none")
        header.append(I+"public")
        header.append("")

        self.nVDW = 1
        self.nGE = 1
        self.nLIJ = 1
        code_lines_vdW = []
        code_lines_GE = []
        code_lines_lij = []
        for bins in self.bin_list:
            #print(bins.filepath)
            bin_code_lines_vdW, bin_code_lines_GE, bin_code_lines_lij = bins.get_fortran_code()
            for line in bin_code_lines_vdW:
                new_line = self.replace_VDW_tag(line)
                code_lines_vdW.append(new_line)
            for line in bin_code_lines_GE:
                new_line = self.replace_GE_tag(line)
                code_lines_GE.append(new_line)
            for line in bin_code_lines_lij:
                new_line = self.replace_LIJ_tag(line)
                code_lines_lij.append(new_line)
        # Correct count to actual number of entries
        self.nVDW -= 1
        self.nGE -= 1
        self.nLIJ -= 1

        # Combine lists
        code_lines = header + code_lines_vdW + code_lines_GE + code_lines_lij

        cl = self.get_VDW_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_GE_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_LIJ_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        code_lines.append("")
        code_lines.append("end module mixdatadb")

        with open(filename, "w") as f:
            for line in code_lines:
                f.write(line)
                f.write("\n")

    def get_array_fortran_code(self,code_lines,nMax,get_tag):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        for i in range(math.ceil(nMax/N_TAGS_PER_LINE)):
            line = 3*I
            jmax = min(nMax-i*N_TAGS_PER_LINE,N_TAGS_PER_LINE)
            for j in range(jmax):
                jj = i*N_TAGS_PER_LINE+j+1
                if j == jmax-1:
                    if jj == nMax:
                        line += get_tag(jj) + " &"
                    else:
                        line += get_tag(jj) + ", &"
                else:
                    line += get_tag(jj) + ","
            code_lines.append(line)
        code_lines.append(I+"/)")
        return code_lines

    def get_VDW_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxkij =" + str(self.nVDW))
        code_lines.append(I+"type (kijdatadb), dimension(maxkij), parameter :: kijdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nVDW,self.get_VDW_tag)
        return code_lines

    def get_GE_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxinterGEij =" + str(self.nGE))
        code_lines.append(I+"type (interGEdatadb), dimension(maxinterGEij), parameter :: interGEdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nGE,self.get_GE_tag)
        return code_lines

    def get_LIJ_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxlij =" + str(self.nLIJ))
        code_lines.append(I+"type (lijdatadb), dimension(maxlij), parameter :: lijdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nLIJ,self.get_LIJ_tag)
        return code_lines

    def get_VDW_tag(self,i_vdw=None):
        """FORTRAN component parameter name
        """
        vdwtag = "vdw"
        if i_vdw is None:
            vdwtag += str(self.nVDW)
        else:
            vdwtag += str(i_vdw)
        return vdwtag

    def replace_VDW_tag(self, line):
        """Replace VDWTAG with FORTRAN component parameter name
        """
        vdwtag = self.get_VDW_tag()
        if "VDWTAG" in line:
            line = line.replace("VDWTAG",vdwtag)
            self.nVDW += 1
        return line

    def get_GE_tag(self,i_ge=None):
        """FORTRAN GE parameter name
        """
        getag = "ge"
        if i_ge is None:
            getag += str(self.nGE)
        else:
            getag += str(i_ge)
        return getag

    def replace_GE_tag(self, line):
        """Replace GETAG with FORTRAN Cp parameter name
        """
        getag = self.get_GE_tag()
        if "GETAG" in line:
            line = line.replace("GETAG",getag)
            self.nGE += 1
        return line

    def get_LIJ_tag(self,i_lij=None):
        """FORTRAN LIJ  parameter name
        """
        lijtag = "lij"
        if i_lij is None:
            lijtag += str(self.nLIJ)
        else:
            lijtag += str(i_lij)
        return lijtag

    def replace_LIJ_tag(self, line):
        """Replace LIJTAG with FORTRAN parameter name
        """
        lijtag = self.get_LIJ_tag()
        if "LIJTAG" in line:
            line = line.replace("LIJTAG",lijtag)
            self.nLIJ += 1
        return line


if __name__ == "__main__":
    binl = binary_list()
    binl.save_fortran_file("mixdatadb.f90")

