"""Module for automatic generation of FORTRAN code of SAFT-VR Mie component data."""
import numpy as np
from sys import exit
import os
import math
import json
from datetime import datetime
from compdata import component, comp_list
from data_utils import I, N_TAGS_PER_LINE, \
    sci_print_float, print_float, saft_eos_to_idx, \
    get_assoc_scheme_parameter
from binarydata import binaries, binary_list
from shutil import copy

class svrm_component(component):
    """Read component data from file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        super(svrm_component, self).__init__(filepath)
        self.eos = "SAFTVRMIE"

    def get_svrm_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type(saftvrmie_data), parameter :: SAFTVRMIETAG = &")
        code_lines.append(3*I+"saftvrmie_data(eosidx = " + saft_eos_to_idx(self.eos) + ", &")
        code_lines.append(3*I+"compName = \"" + self.comp["ident"] + "\", &")
        code_lines.append(3*I + 'm = {}'.format(print_float(self.comp[tag]["m"])) + ", &")
        code_lines.append(3*I + 'sigma = {}'.format(sci_print_float(self.comp[tag]["sigma"])) + ", &")
        code_lines.append(3*I + 'eps_depth_divk = {}'.format(print_float(self.comp[tag]["eps_depth_divk"])) + ", &")
        code_lines.append(3*I + 'lambda_a = {}'.format(print_float(self.comp[tag]["lambda_a"])) + ", &")
        code_lines.append(3*I + 'lambda_r = {}'.format(print_float(self.comp[tag]["lambda_r"])) + ", &")
        code_lines.append(3*I + 'mass = {}'.format(sci_print_float(self.comp[tag]["mass"])) + ", &")
        code_lines.append(3*I + 'eps = {}'.format(print_float(self.comp[tag]["eps"])) + ", &")
        code_lines.append(3*I + 'beta = {:.4E}'.format(self.comp[tag]["beta"]) + ", &")
        code_lines.append(3*I + 'assoc_scheme = {}, &'.format(
            get_assoc_scheme_parameter(self.comp[tag]["assoc_scheme"])))
        code_lines.append(3*I + 'fh_order = {}'.format(str(self.comp[tag]["fh_order"])) + ", &")
        code_lines.append(3*I + "bib_ref = \"" + self.comp[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I + "ref = \"" + self.comp[tag]["ref"] + "\" &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """

        code_lines = []
        for key in self.comp:
            if "SAFTVRMIE" in key:
                svrm = self.get_svrm_fortran_code(key)
                for line in svrm:
                    code_lines.append(line)

        return code_lines


class svrm_comp_list(comp_list):
    """Read component data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None, comp=svrm_component):
        super(svrm_comp_list, self).__init__(path=path, comp=comp)
        self.nSVRM = None

    def get_fortran_code(self):
        """Generate component data fortran file input for Thermopack
        """
        code_lines = []

        self.nSVRM = 1
        for comp in self.comp_list:
            comp_code_lines = comp.get_fortran_code()
            for il, line in enumerate(comp_code_lines):
                new_line = line
                new_line = self.replace_svrm_tag(new_line)
                code_lines.append(new_line)
        # Correct count to actual number of entries
        self.nSVRM -= 1

        cl = self.get_comp_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        return code_lines

    def get_comp_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"integer, parameter :: nMiemodels = " + str(self.nSVRM))
        code_lines.append(I+"type(saftvrmie_data), dimension(nMiemodels), parameter :: Miearray = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nSVRM,self.get_svrm_tag)
        code_lines.append("")
        return code_lines

    def get_svrm_tag(self,i_svrm=None):
        """FORTRAN SAFTVRMIE parameter name
        """
        svrmtag = "Miecx"
        if i_svrm is None:
            svrmtag += str(self.nSVRM)
        else:
            svrmtag += str(i_svrm)
        return svrmtag

    def replace_svrm_tag(self, line):
        """Replace TAG with parameter name
        """
        svrmtag = self.get_svrm_tag()
        if "SAFTVRMIETAG" in line:
            line = line.replace("SAFTVRMIETAG",svrmtag)
            self.nSVRM += 1
        return line

class svrm_binaries(binaries):
    """Read binary data form file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        super(svrm_binaries, self).__init__(filepath)
        self.eos = "SAFTVRMIE"

    def get_saftvrmie_xij_fortran_code(self, tag):
        """
        Input:
        tag
        Output:
        code_lines - Code lines
        """
        interaction = tag[0]
        TAG = interaction.upper() + "IJSAFTVRMIETAG"
        xij = interaction.lower() + "ij"
        code_lines = []
        code_lines.append(I+"type (Miekijdata), parameter :: " + TAG + " = &")
        code_lines.append(3*I+"Miekijdata(eosidx = " + saft_eos_to_idx(self.eos) + ", &")
        code_lines.append(3*I+"ref = \"" + self.bins[tag]["ref"] + "\", &")
        code_lines.append(3*I+"bib_ref = \"" + self.bins[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I+"uid1 = \"" + self.bins[tag]["uid1"] + "\", &")
        code_lines.append(3*I+"uid2 = \"" + self.bins[tag]["uid2"] + "\", &")
        code_lines.append(3*I + 'kijvalue = {}'.format(print_float(self.bins[tag][xij])) + "  &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_SVRM_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """
        code_lines_saftvrmie = []
        for key in self.bins:
            if self.mixing == "SAFTVRMIE":
                cl = self.get_saftvrmie_xij_fortran_code(key)
                code_lines_saftvrmie += cl
        return code_lines_saftvrmie


class svrm_binary_list(binary_list):
    """Read binary data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None, binclass=svrm_binaries):
        """Load files from data-base
        Arguments:
        filename - path to folder
        """
        super(svrm_binary_list, self).__init__(path, binclass)

    def get_fortran_code(self):
        """
        """

        self.nSVRM_KIJ = 1
        self.nSVRM_LIJ = 1
        code_lines_saftvrmie = []
        for bins in self.bin_list:
            #print(bins.filepath)
            bin_code_lines_saftvrmie = bins.get_SVRM_fortran_code()
            for line in bin_code_lines_saftvrmie:
                new_line = self.replace_SVRM_tag(line)
                code_lines_saftvrmie.append(new_line)

        # Correct count to actual number of entries
        self.nSVRM_KIJ -= 1
        self.nSVRM_LIJ -= 1

        # Combine lists
        code_lines = code_lines_saftvrmie

        cl = self.get_SVRM_KIJ_array_fortran_code()
        code_lines += cl
        cl = self.get_SVRM_LIJ_array_fortran_code()
        code_lines += cl

        return code_lines

    def get_SVRM_KIJ_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"integer, parameter :: Miemaxkij = " + str(self.nSVRM_KIJ))
        code_lines.append(I+"type (Miekijdata), dimension(Miemaxkij), parameter :: Miekijdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nSVRM_KIJ,self.get_SVRM_KIJ_tag)
        code_lines.append("")
        return code_lines

    def get_SVRM_LIJ_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"integer, parameter :: Miemaxlij = " + str(self.nSVRM_LIJ))
        code_lines.append(I+"type (Miekijdata), dimension(Miemaxlij), parameter :: Mielijdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nSVRM_LIJ,self.get_SVRM_LIJ_tag)
        code_lines.append("")
        return code_lines

    def replace_SVRM_tag(self, line):
        """Replace SAFT-VR Mie tags  with FORTRAN component parameter name
        """
        if "KIJSAFTVRMIETAG" in line:
            line = line.replace("KIJSAFTVRMIETAG",self.get_SVRM_KIJ_tag())
            self.nSVRM_KIJ += 1
        elif "LIJSAFTVRMIETAG" in line:
            line = line.replace("LIJSAFTVRMIETAG",self.get_SVRM_LIJ_tag())
            self.nSVRM_LIJ += 1
        return line

    def get_SVRM_KIJ_tag(self,i_SVRM=None):
        """FORTRAN SVRM tag parameter name
        """
        svrmtag = "SVRM_KIJ_"
        if i_SVRM is None:
            svrmtag += str(self.nSVRM_KIJ)
        else:
            svrmtag += str(i_SVRM)
        return svrmtag

    def get_SVRM_LIJ_tag(self,i_SVRM=None):
        """FORTRAN SVRM tag parameter name
        """
        svrmtag = "SVRM_LIJ_"
        if i_SVRM is None:
            svrmtag += str(self.nSVRM_LIJ)
        else:
            svrmtag += str(i_SVRM)
        return svrmtag

def SVRM_class_definition():
    classes = []
    classes.append(I+"!> PURE COMPONENT PARAMETERS.")
    classes.append(I+"!> This data structure stores pure component parameters for the")
    classes.append(I+"!> SAFT-VRQ Mie EoS")
    classes.append(I+"! ---------------------------------------------------------------------------")
    classes.append(I+"type :: saftvrmie_data")
    classes.append(2*I+"integer :: eosidx")
    classes.append(2*I+"character (len=uid_len) :: compName")
    classes.append(2*I+"! Pure component fitted parameters.")
    classes.append(2*I+"real :: m        !< [-]. Mean number of segments.")
    classes.append(2*I+"real :: sigma    !< [m]. Temperature-independent segment diameter.")
    classes.append(2*I+"real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.")
    classes.append(2*I+"real :: lambda_a !< [] attractive exponent of the Mie potential")
    classes.append(2*I+"real :: lambda_r !< [] repulsive exponent of the Mie potential")
    classes.append(2*I+"real :: mass     !< Segment mass, i.e. molecule mass dividided by number of segments [kg]")
    classes.append(2*I+"! Association parameters.")
    classes.append(2*I+"real :: eps  !< [J/mol].")
    classes.append(2*I+"real :: beta !< [-]. Also known as kappa in SAFT literature.")
    classes.append(2*I+"!real :: rc  !< [m]. range of association")
    classes.append("")
    classes.append(2*I+"! Association scheme.")
    classes.append(2*I+"integer :: assoc_scheme")
    classes.append(2*I+"! Order of Feynman--Hibbs correction (nonzero only for quantum fluids).")
    classes.append(2*I+"integer :: fh_order")
    classes.append(2*I+"! Bibliograpic reference")
    classes.append(2*I+"character(len=bibref_len) :: bib_ref")
    classes.append(2*I+"! Parameter set")
    classes.append(2*I+"character(len=ref_len) :: ref")
    classes.append(I+"end type saftvrmie_data")
    classes.append("")
    classes.append(I+"!> INTERACTION PARAMETERS FOR THE SAFT-VR-MIE DISPERSION TERM.")
    classes.append(I+"! ----------------------------------------------------------------------------")
    classes.append(I+"type :: Miekijdata")
    classes.append(2*I+"integer:: eosidx")
    classes.append(2*I+"character (len=uid_len) :: uid1, uid2")
    classes.append(2*I+"character(len=ref_len) :: ref ! Parameter set")
    classes.append(2*I+"character(len=bibref_len) :: bib_ref ! Bibliographic reference")
    classes.append(2*I+"real :: kijvalue")
    classes.append(I+"end type Miekijdata")
    classes.append("")

    return classes

def SVRM_header_and_footer():
        """

        """
        header = []
        header.append("!> Automatically generated to file saftvrmie_datadb.f90")
        header.append("!! using utility python code pyUtils")
        now = datetime.today().isoformat()
        header.append("!! Time stamp: " + now)
        header.append("")
        header.append("module saftvrmie_datadb")
        header.append(I+"use thermopack_constants, only: uid_len, ref_len, bibref_len")
        header.append(I+"use AssocSchemeUtils")
        header.append(I+"use eosdata, only: eosSAFT_VR_MIE")
        header.append(I+"implicit none")
        header.append(I+"public")
        header.append("")

        classes = SVRM_class_definition()
        header += classes

        footer = ["end module saftvrmie_datadb"]

        return header, footer

def save_SVRM_fortran_file(code_lines):
        """ Save saftvrmie_datadb.f90
        """
        with open("saftvrmie_datadb.f90", "w") as f:
            for line in code_lines:
                f.write(line)
                f.write("\n")


if __name__ == "__main__":
    # Read json files
    comps = svrm_comp_list()
    binl = svrm_binary_list()
    # Get header and footer
    header, footer = SVRM_header_and_footer()
    code_lines = header
    comp_code = comps.get_fortran_code()
    code_lines += comp_code
    bin_code = binl.get_fortran_code()
    code_lines += bin_code
    code_lines += footer
    save_SVRM_fortran_file(code_lines)
    copy('saftvrmie_datadb.f90', '../../../src/saftvrmie_datadb.f90')
