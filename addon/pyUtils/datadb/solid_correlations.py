"""Module for automatic generation of FORTRAN code of PC-SAFT component and binary data."""
import numpy as np
from sys import exit
import os
import math
import json
from compdata import component, comp_list
from datetime import datetime
from data_utils import I, N_TAGS_PER_LINE, \
    sci_print_float, print_float, \
    get_assoc_scheme_parameter
from shutil import copy


class solid_correlation(component):
    """Read component data from file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        super(solid_correlation, self).__init__(filepath)

    def get_solid_correlation_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """

        code_lines = []
        code_lines.append(I+"type(solid_correlation_data), parameter :: CORRTAG = &")
        code_lines.append(3*I+"solid_correlation_data( &")
        code_lines.append(3*I+"compName = \"" + self.fluid["ident"] + "\", &")
        code_lines.append(3*I+"correlation = \"" + self.fluid[tag]["correlation"] + "\", &")
        code_lines.append(
            3*I + 'triple_temperature = {}'.format(print_float(self.fluid[tag]["triple_temperature"])) + ", &")
        maximum_temperature = self.fluid[tag]["maximum_temperature"] if tag == "melting_curve" else "0.0"
        minimum_temperature = self.fluid[tag]["minimum_temperature"] if tag == "sublimation_curve" else "0.0"
        code_lines.append(
            3*I + 'minimum_temperature = {}'.format(print_float(float(minimum_temperature))) + ", &")
        code_lines.append(
            3*I + 'maximum_temperature = {}'.format(print_float(float(maximum_temperature))) + ", &")
        code_lines.append(
            3*I + 'reducing_pressure = {}'.format(print_float(self.fluid[tag]["reducing_pressure"])) + ", &")
        code_lines.append(
            3*I + 'reducing_temperature = {}'.format(print_float(self.fluid[tag]["reducing_temperature"])) + ", &")
        code_lines.append(
            3*I + 'n_coeff = {}'.format(self.fluid[tag]["n_coeff"]) + ", &")
        code_lines.append(
            3*I + 'n_coeff_1 = {}'.format(self.fluid[tag]["n_coeff_1"]) + ", &")
        code_lines.append(
            3*I + 'n_coeff_2 = {}'.format(self.fluid[tag]["n_coeff_2"]) + ", &")
        code_lines.append(
            3*I + 'n_coeff_3 = {}'.format(self.fluid[tag]["n_coeff_3"]) + ", &")
        code_lines.append(3*I + "coeff = (/" + '{:.12e}'.format(self.fluid[tag]["coeff"][0]) + "," + '{:.12e}'.format(self.fluid[tag]["coeff"][1])
                          + "," + '{:.12e}'.format(self.fluid[tag]["coeff"][2]) + ", &")
        code_lines.append(3*I + '{:.12e}'.format(self.fluid[tag]["coeff"][3]) + "," + '{:.12e}'.format(self.fluid[tag]["coeff"][4])
                          + "," + '{:.12e}'.format(self.fluid[tag]["coeff"][5]) + "/), &")
        code_lines.append(3*I + "exponents = (/" + '{:.6f}'.format(self.fluid[tag]["exponents"][0]) + "," + '{:.6f}'.format(self.fluid[tag]["exponents"][1])
                          + "," + '{:.6f}'.format(self.fluid[tag]["exponents"][2]) + ", &")
        code_lines.append(3*I + '{:.6f}'.format(self.fluid[tag]["exponents"][3]) + "," + '{:.6f}'.format(self.fluid[tag]["exponents"][4])
                          + "," + '{:.6f}'.format(self.fluid[tag]["exponents"][5]) + "/), &")
        code_lines.append(3*I + "bib_ref = \"" +
                          self.fluid[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I + "ref = \"" + self.fluid[tag]["ref"] + "\" &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """

        code_lines = []
        for key in self.fluid:
            if tag in key:
                print(self.filepath)
                corr = self.get_solid_correlation_fortran_code(key)
                for line in corr:
                    code_lines.append(line)

        return code_lines


class solid_correlation_comp_list(comp_list):
    """Read component data files into list and generate
    component data file for Thermopack
    """

    def __init__(self, path=None, comp=solid_correlation):
        super(solid_correlation_comp_list, self).__init__(path=path, comp=comp)
        self.n_corr_melt = 0
        self.n_corr_sub = 0

    def get_fortran_code(self):
        """Generate component data fortran file input for Thermopack
        """
        code_lines = []

        self.n_corr_melt = 0
        for comp in self.comp_list:
            mc_code_lines = comp.get_fortran_code("melting_curve")
            for il, line in enumerate(mc_code_lines):
                new_line = line
                if "CORRTAG" in line:
                    self.n_corr_melt += 1
                    new_line = new_line.replace("CORRTAG","MELT"+str(self.n_corr_melt))
                code_lines.append(new_line)

        self.n_corr_sub = 0
        for comp in self.comp_list:
            sc_code_lines = comp.get_fortran_code("sublimation_curve")
            for il, line in enumerate(sc_code_lines):
                new_line = line
                if "CORRTAG" in line:
                    self.n_corr_sub += 1
                    new_line = new_line.replace("CORRTAG","SUBL"+str(self.n_corr_sub))
                code_lines.append(new_line)

        cl = self.get_comp_array_fortran_code(is_melting=True)
        for line in cl:
            code_lines.append(line)
        cl = self.get_comp_array_fortran_code(is_melting=False)
        for line in cl:
            code_lines.append(line)

        return code_lines

    def get_comp_array_fortran_code(self, is_melting):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        if is_melting:
            array_prefix = "melting_corr_"
            n_var = "n_melting_curves"
            n_num = self.n_corr_melt
            get_tag = self.get_melting_tag
        else:
            array_prefix = "sublimation_corr_"
            n_var = "n_sublimation_curves"
            n_num = self.n_corr_sub
            get_tag = self.get_sublimation_tag
        code_lines = []
        code_lines.append(
            I+"integer, parameter :: "+n_var+" = " + str(n_num))
        code_lines.append(
            I+"type(solid_correlation_data), dimension("+n_var+"), parameter :: "+array_prefix+"array = (/&")
        code_lines = self.get_array_fortran_code(code_lines, n_num, get_tag)
        code_lines.append("")
        return code_lines

    def get_melting_tag(self,i):
        """FORTRAN parameter name
        """
        return "MELT" + str(i)

    def get_sublimation_tag(self,i):
        """FORTRAN parameter name
        """
        return "SUBL" + str(i)


def solid_correlation_datadb_class_definition():
    classes = []
    classes.append(I+"!> This data structure stores parameters for")
    classes.append(I+"!> sublimation and melting line correlations.")
    classes.append(I+"! ---------------------------------------------------------------------------")
    classes.append(I+"type :: solid_correlation_data")
    classes.append(2*I+"character(len=uid_len) :: compName")
    classes.append(2*I+"character(len=4) :: correlation !< Correlations type")
    classes.append(2*I+"real :: triple_temperature  !< [K]. Triple point temperature.")
    classes.append(2*I+"real :: minimum_temperature  !< [K]. Minimum temperature for sublimation line.")
    classes.append(2*I+"real :: maximum_temperature  !< [K]. Maximum temperature for melting line.")
    classes.append(2*I+"real :: reducing_pressure !< [Pa]. Pressure scaling parameter.")
    classes.append(2*I+"real :: reducing_temperature !< [K]. Temperature reducing parameter.")
    classes.append(2*I+"integer :: n_coeff !< Number of coefficients")
    classes.append(2*I+"integer :: n_coeff_1 !< Number of coefficients for type one terms")
    classes.append(2*I+"integer :: n_coeff_2 !< Number of coefficients for type two terms")
    classes.append(2*I+"integer :: n_coeff_3 !< Number of coefficients for type three terms")
    classes.append(2*I+"real :: coeff(6)  !< Correlation coefficients")
    classes.append(2*I+"real :: exponents(6) !< Correlation exponents.")
    classes.append(2*I+"character(len=bibref_len) :: bib_ref !< Bibliograpic reference.")
    classes.append(2*I+"character(len=ref_len) :: ref !< Parameter set")
    classes.append(I+"end type solid_correlation_data")
    classes.append("")

    return classes

def solid_correlation_datadb_header_and_footer():
        """

        """
        header = []
        header.append("!> Automatically generated to file solid_correlation_datadb.f90")
        header.append("!! using utility python code pyUtils")
        now = datetime.today().isoformat()
        header.append("!! Time stamp: " + now)
        header.append("")
        header.append("module solid_correlation_datadb")
        header.append(I+"use thermopack_constants, only: uid_len, ref_len, bibref_len")
        header.append(I+"implicit none")
        header.append(I+"public")
        header.append("")

        classes = solid_correlation_datadb_class_definition()
        header += classes

        footer = ["end module solid_correlation_datadb"]

        return header, footer

def save_solid_correlation_fortran_file(code_lines):
        """ Save solid_correlation_datadb.f90
        """
        with open("solid_correlation_datadb.f90", "w") as f:
            for line in code_lines:
                f.write(line)
                f.write("\n")


if __name__ == "__main__":
    # Read json files
    comps = solid_correlation_comp_list()
    # Get header and footer
    header, footer = solid_correlation_datadb_header_and_footer()
    code_lines = header
    comp_code = comps.get_fortran_code()
    code_lines += comp_code
    code_lines += footer
    save_solid_correlation_fortran_file(code_lines)
    copy('solid_correlation_datadb.f90', '../../../src/solid_correlation_datadb.f90')
