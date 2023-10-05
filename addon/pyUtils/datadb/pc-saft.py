"""Module for automatic generation of FORTRAN code of PC-SAFT component and binary data."""
import numpy as np
from sys import exit
import os
import math
import json
from datetime import datetime
from compdata import component, comp_list
from data_utils import I, N_TAGS_PER_LINE, \
    sci_print_float, print_float, saft_eos_to_idx, \
    get_assoc_scheme_parameter, get_mix_model_parameter
from binarydata import binaries, binary_list
from shutil import copy


class pcsaft_component(component):
    """Read component data from file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        super(pcsaft_component, self).__init__(filepath)
        self.eos = "PC-SAFT"

    def get_pcsaft_fortran_code(self, tag):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type(pc_saft_data), parameter :: PC-SAFTTAG = &")
        code_lines.append(3*I+"pc_saft_data(eosidx = " +
                          saft_eos_to_idx(self.eos) + ", &")
        code_lines.append(3*I+"compName = \"" + self.comp["ident"] + "\", &")
        code_lines.append(
            3*I + 'm = {}'.format(print_float(self.comp[tag]["m"])) + ", &")
        code_lines.append(
            3*I + 'sigma = {}'.format(sci_print_float(self.comp[tag]["sigma"])) + ", &")
        code_lines.append(
            3*I + 'eps_depth_divk = {}'.format(print_float(self.comp[tag]["eps_depth_divk"])) + ", &")
        code_lines.append(
            3*I + 'eps = {}'.format(print_float(self.comp[tag]["eps"])) + ", &")
        code_lines.append(
            3*I + 'beta = {}'.format(print_float(self.comp[tag]["beta"])) + ", &")
        code_lines.append(3*I + 'assoc_scheme = {}, &'.format(
            get_assoc_scheme_parameter(self.comp[tag]["assoc_scheme"])))
        mu = self.comp[tag]["mu"] if "mu" in self.comp[tag] else 0.0
        code_lines.append(3*I + 'mu = {}, &'.format(print_float(mu)))
        Q = self.comp[tag]["Q"] if "Q" in self.comp[tag] else 0.0
        code_lines.append(3*I + 'Q = {}, &'.format(print_float(Q)))
        code_lines.append(3*I + "bib_ref = \"" +
                          self.comp[tag]["bib_reference"] + "\", &")
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
            if "PC-SAFT" in key:
                pcsaft = self.get_pcsaft_fortran_code(key)
                for line in pcsaft:
                    code_lines.append(line)

        return code_lines


class pcsaft_comp_list(comp_list):
    """Read component data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None, comp=pcsaft_component):
        super(pcsaft_comp_list, self).__init__(path=path, comp=comp)
        self.nPCSAFT = None

    def get_fortran_code(self):
        """Generate component data fortran file input for Thermopack
        """
        code_lines = []

        self.nPCSAFT = 1
        for comp in self.comp_list:
            comp_code_lines = comp.get_fortran_code()
            for il, line in enumerate(comp_code_lines):
                new_line = line
                new_line = self.replace_pcsaft_tag(new_line)
                code_lines.append(new_line)
        # Correct count to actual number of entries
        self.nPCSAFT -= 1

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
        code_lines.append(
            I+"integer, parameter :: nPCmodels = " + str(self.nPCSAFT))
        code_lines.append(
            I+"type(pc_saft_data), dimension(nPCmodels), parameter :: PCarray = (/&")
        code_lines = self.get_array_fortran_code(
            code_lines, self.nPCSAFT, self.get_pcsaft_tag)
        code_lines.append("")
        return code_lines

    def get_pcsaft_tag(self,i_pcsaft=None):
        """FORTRAN PC-SAFT parameter name
        """
        pcsafttag = "PCcx"
        if i_pcsaft is None:
            pcsafttag += str(self.nPCSAFT)
        else:
            pcsafttag += str(i_pcsaft)
        return pcsafttag

    def replace_pcsaft_tag(self, line):
        """Replace TAG with parameter name
        """
        pcsafttag = self.get_pcsaft_tag()
        if "PC-SAFTTAG" in line:
            line = line.replace("PC-SAFTTAG",pcsafttag)
            self.nPCSAFT += 1
        return line

class pcsaft_binaries(binaries):
    """Read binary data form file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        super(pcsaft_binaries, self).__init__(filepath)
        self.eos = "PC-SAFT"

    def get_pc_saft_kij_fortran_code(self, tag):
        """
        Input:
        tag
        Output:
        code_lines - Code lines
        """
        interaction = tag[0]
        TAG = interaction.upper() + "IJ_PC-SAFTTAG"
        xij = interaction.lower() + "ij"
        code_lines = []
        code_lines.append(I+"type (PCkijdata), parameter :: " + TAG + " = &")
        code_lines.append(3*I+"PCkijdata(eosidx = " + saft_eos_to_idx(self.eos) + ", &")
        code_lines.append(3*I+"ref = \"" + self.bins[tag]["ref"] + "\", &")
        code_lines.append(3*I+"bib_ref = \"" + self.bins[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I+"uid1 = \"" + self.bins[tag]["uid1"] + "\", &")
        code_lines.append(3*I+"uid2 = \"" + self.bins[tag]["uid2"] + "\", &")
        code_lines.append(3*I + 'kijvalue = {}'.format(print_float(self.bins[tag][xij])) + ", &")
        if "eps_comb_rule" in self.bins[tag].keys():
            eps_comb_rule = self.bins[tag]["eps_comb_rule"]
        else:
            eps_comb_rule = "DEFAULT"
        if "beta_comb_rule" in self.bins[tag].keys():
            beta_comb_rule = self.bins[tag]["beta_comb_rule"]
        else:
            beta_comb_rule = "DEFAULT"
        code_lines.append(3*I + 'eps_comb_rule = {}'.format(get_mix_model_parameter(eps_comb_rule)) + ",  &")
        code_lines.append(3*I + 'beta_comb_rule = {}'.format(get_mix_model_parameter(beta_comb_rule)) + "  &")        
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_PCSAFT_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """
        code_lines_pc_saft = []
        for key in self.bins:
            if self.mixing == "PC-SAFT":
                cl = self.get_pc_saft_kij_fortran_code(key)
                code_lines_pc_saft += cl
        return code_lines_pc_saft


class pcsaft_binary_list(binary_list):
    """Read binary data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None, binclass=pcsaft_binaries):
        """Load files from data-base
        Arguments:
        filename - path to folder
        """
        super(pcsaft_binary_list, self).__init__(path, binclass)
        self.nPCSAFT_KIJ = None

    def get_fortran_code(self):
        """
        """

        self.nPCSAFT_KIJ = 1
        code_lines_pc_saft = []
        for bins in self.bin_list:
            #print(bins.filepath)
            bin_code_lines_pc_saft = bins.get_PCSAFT_fortran_code()
            for line in bin_code_lines_pc_saft:
                new_line = self.replace_PCSAFT_tag(line)
                code_lines_pc_saft.append(new_line)

        # Correct count to actual number of entries
        self.nPCSAFT_KIJ -= 1

        # Combine lists
        code_lines = code_lines_pc_saft

        cl = self.get_PCSAFT_KIJ_array_fortran_code()
        code_lines += cl

        return code_lines

    def get_PCSAFT_KIJ_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"integer, parameter :: PCmaxkij = " + str(self.nPCSAFT_KIJ))
        code_lines.append(I+"type (PCkijdata), dimension(PCmaxkij), parameter :: PCkijdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nPCSAFT_KIJ,self.get_PCSAFT_KIJ_tag)
        code_lines.append("")
        return code_lines

    def replace_PCSAFT_tag(self, line):
        """Replace PC-SAFT tags  with FORTRAN component parameter name
        """
        if "KIJ_PC-SAFTTAG" in line:
            line = line.replace("KIJ_PC-SAFTTAG",self.get_PCSAFT_KIJ_tag())
            self.nPCSAFT_KIJ += 1
        return line

    def get_PCSAFT_KIJ_tag(self,i_PCSAFT=None):
        """FORTRAN PC-SAFT tag parameter name
        """
        pcsafttag = "PCSAFT_KIJ_"
        if i_PCSAFT is None:
            pcsafttag += str(self.nPCSAFT_KIJ)
        else:
            pcsafttag += str(i_PCSAFT)
        return pcsafttag


def PCSAFT_class_definition():
    classes = []
    classes.append(I+"!> PURE COMPONENT PARAMETERS.")
    classes.append(I+"!> This data structure stores pure component parameters for the")
    classes.append(I+"!> PC-SAFT equation of state.")
    classes.append(I+"! ---------------------------------------------------------------------------")
    classes.append(I+"type :: pc_saft_data")
    classes.append(2*I+"integer :: eosidx")
    classes.append(2*I+"character(len=uid_len) :: compName")
    classes.append(2*I+"! Pure component fitted parameters.")
    classes.append(2*I+"real :: m        !< [-]. Mean number of segments.")
    classes.append(2*I+"real :: sigma    !< [m]. Temperature-independent segment diameter.")
    classes.append(2*I+"real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.")
    classes.append(2*I+"! Association parameters.")
    classes.append(2*I+"real :: eps  !< [J/mol].")
    classes.append(2*I+"real :: beta !< [-]. Also known as kappa in SAFT literature.")
    classes.append(2*I+"integer :: assoc_scheme !< Association scheme.")
    classes.append(2*I+"! Electical moment parameters.")
    classes.append(2*I+"real :: mu  !< Dipole-moment [D]")
    classes.append(2*I+"real :: Q !< Quadrupol-moment [ÅD]")
    classes.append(2*I+"! Bibliograpic reference")
    classes.append(2*I+"character(len=bibref_len) :: bib_ref")
    classes.append(2*I+"! Parameter set")
    classes.append(2*I+"character(len=ref_len) :: ref")
    classes.append(I+"end type pc_saft_data")
    classes.append("")
    classes.append(I+"!> TEMPERATURE-INDEPENDENT INTERACTION PARAMETERS FOR PC-SAFT DISPERSION TERM.")
    classes.append(I+"! ----------------------------------------------------------------------------")
    classes.append(I+"type :: PCkijdata")
    classes.append(2*I+"integer:: eosidx")
    classes.append(2*I+"character(len=uid_len) :: uid1, uid2")
    classes.append(2*I+"character(len=ref_len) :: ref ! Parameter set")
    classes.append(2*I+"character(len=bibref_len) :: bib_ref ! Bibliographic reference")
    classes.append(2*I+"real :: kijvalue")
    classes.append(2*I+"integer :: eps_comb_rule")
    classes.append(2*I+"integer :: beta_comb_rule")
    classes.append(I+"end type PCkijdata")
    classes.append("")

    return classes

def PCSAFT_header_and_footer():
        """

        """
        header = []
        header.append("!> Automatically generated to file pc_saft_datadb.f90")
        header.append("!! using utility python code pyUtils")
        now = datetime.today().isoformat()
        header.append("!! Time stamp: " + now)
        header.append("")
        header.append("module pc_saft_datadb")
        header.append(I+"use thermopack_constants, only: uid_len, ref_len, bibref_len")
        header.append(I+"use AssocSchemeUtils")
        header.append(I+"use eosdata, only: eosPC_SAFT, eosOPC_SAFT, eosSPC_SAFT")
        header.append(I+"implicit none")
        header.append(I+"public")
        header.append("")

        classes = PCSAFT_class_definition()
        header += classes

        footer = ["end module pc_saft_datadb"]

        return header, footer

def save_PCSAFT_fortran_file(code_lines):
        """ Save pc_saft_datadb.f90
        """
        with open("pc_saft_datadb.f90", "w") as f:
            for line in code_lines:
                f.write(line)
                f.write("\n")


if __name__ == "__main__":
    # Read json files
    comps = pcsaft_comp_list()
    binl = pcsaft_binary_list()
    # Get header and footer
    header, footer = PCSAFT_header_and_footer()
    code_lines = header
    comp_code = comps.get_fortran_code()
    code_lines += comp_code
    bin_code = binl.get_fortran_code()
    code_lines += bin_code
    code_lines += footer
    save_PCSAFT_fortran_file(code_lines)
    copy('pc_saft_datadb.f90', '../../../src/pc_saft_datadb.f90')
