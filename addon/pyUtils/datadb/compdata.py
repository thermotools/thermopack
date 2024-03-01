"""Module for automatic generation of FORTRAN code of component data."""
from __future__ import print_function
import numpy as np
import sys
import os
import math
import json
from datetime import datetime
from data_utils import I, N_TAGS_PER_LINE, \
    sci_print_float, print_float, \
    get_assoc_scheme_parameter, \
    get_alpha_index_parameter
from shutil import copy
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'docs'))
import tools
from fluid_file import fluid_file
from entropy import entropy_references, RGAS

def get_keys_from_base_name(d,name):
    key_list = []
    for key in d.keys():
        if name in key:
            key_list.append(key)
    return key_list

class component(fluid_file):
    """Read component data and generate component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        super().__init__(filepath)
        self.eos_list = ["PR", "SRK", "CSP-SRK", "CSP-PR",
                         "CPA-SRK", "CPA-SRK", "LK", "PC-SAFT"
                         "SAFT-VR-MIE", "RK", "VDW", "SW", "PT"]

        self.dgf = None
        # Make dictionary of elements
        self.elements = {}
        elements = self.fluid["structure"].split(";")
        for elem in elements:
            e = elem.split(":")[0].upper()
            if len(e) > 0:
                self.elements[e] = int(elem.split(":")[1])

    @property
    def element_list(self):
        """Get list of elements
        """
        return [k for k in self.elements]

    def convert_entropy_reference_state(self):
        """ Convert to 1Bar reference
        """
        S0 = float(self.fluid["reference_state"]["entropy"])
        S0_ref = self.fluid["reference_state"]["entropy_ref_state"]
        if S0_ref == "1BAR":
            S0_ref_pressure = 1.0e5
        elif S0_ref == "1ATM":
            S0_ref_pressure = 101325.0
        else:
            raise ValueError("Wrong reference state for entropy")
        S0 = S0 + RGAS*np.log(S0_ref_pressure/1.0e5)
        return S0

    def get_n_TWU(self):
        """Set Mie potential paramaters
        Input:
        eos - String defining EOS
        Output:
        n_TWU - Number of TWU entries
        """
        n_TWU = 0
        for eos in self.eos_list:
            d = self.fluid[eos]
            n_TWU += len(get_keys_from_base_name(d,"TWU"))
        return n_TWU

    def get_n_MC(self):
        """Set Mie potential paramaters
        Input:
        eos - String defining EOS
        Output:
        n_TWU - Number of TWU entries
        """
        n_MC = 0
        for eos in self.eos_list:
            d = self.fluid[eos]
            n_MC += len(get_keys_from_base_name(d,"MC"))
        return n_MC

    def get_comp_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type (gendatadb), parameter :: COMPTAG = &")
        code_lines.append(3*I+"gendatadb(ident = \"" + self.fluid["ident"] + "\", &")
        code_lines.append(3*I+"formula = \"" + self.fluid["formula"] + "\", &")
        code_lines.append(3*I+"name = \"" + self.fluid["name"] + "\", &")
        #code_lines.append(3*I+"\"ref=" + self.fluid["ref"] + "\", &")
        code_lines.append(3*I+"structure = \"" + self.fluid["structure"] + "\", &")
        code_lines.append(3*I + 'mw = {:.4f}'.format(self.fluid["mol_weight"]) + ", &")
        code_lines.append(3*I + 'Tc = {:.4f}'.format(self.fluid["critical"]["temperature"]) + ", &")
        code_lines.append(3*I + 'Pc = {:.2f}'.format(self.fluid["critical"]["pressure"]) + ", &")
        code_lines.append(3*I + 'Zc = {:.6f}'.format(self.fluid["critical"]["compressibility"]) + ", &")
        code_lines.append(3*I + 'acf = {:.6f}'.format(self.fluid["acentric_factor"]["acf"]) + ", &")
        code_lines.append(3*I + 'Tb = {:.4f}'.format(self.fluid["boiling_temperature"]["temperature"]) + ", &")
        code_lines.append(3*I + 'Ttr = {:.4f}'.format(self.fluid["triple"]["temperature"]) + ", &")
        code_lines.append(3*I + 'Ptr = {:.4f}'.format(self.fluid["triple"]["pressure"]) + ", &")
        code_lines.append(3*I + 'sref = {:.4f}'.format(self.fluid["reference_state"]["entropy"]) + ", &")
        code_lines.append(3*I + f'sref_state = \"{self.fluid["reference_state"]["entropy_ref_state"]}\", &')
        code_lines.append(3*I + 'href = {:.8e}'.format(self.fluid["reference_state"]["enthalpy"]) + ", &")
        code_lines.append(3*I + 'gref = {:.8e}'.format(self.dGf) + ", &")
        code_lines.append(3*I + "psatcode = " + str(self.fluid["saturation"]["correlation"]) + ", &")
        code_lines.append(3*I + 'ant = (/{:.8e}, {:.8e}, {:.8e}/)'.format(self.fluid["saturation"]["ant"][0], self.fluid["saturation"]["ant"][1], self.fluid["saturation"]["ant"][2]) + ", &")
        code_lines.append(3*I + 'Tantmin = {:.4f}'.format(self.fluid["saturation"]["Tmin"]) + ", &")
        code_lines.append(3*I + 'Tantmax = {:.4f}'.format(self.fluid["saturation"]["Tmax"]) + ", &")
        code_lines.append(3*I + 'Zra = {:.6f}'.format(self.fluid["rackett_factor"]["Zra"]) + ", &")
        code_lines.append(3*I + 'mu_dipole = {:.6f}'.format(self.fluid["dipole_moment"]["mu"]) + ", &")
        code_lines.append(3*I + 'q_quadrupole = {:.6f}'.format(self.fluid["quadrupole_moment"]["Q"]) + " &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_cp_fortran_code(self,tag):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type (cpdata), parameter :: CPTAG" + " = &")
        code_lines.append(3*I+"cpdata(cid = \"" + self.fluid["ident"] + "\", &")
        code_lines.append(3*I+"ref = \"" + self.fluid[tag]["ref"] + "\", &")
        code_lines.append(3*I+"bib_ref = \"" + self.fluid[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I + "cptype = " + str(self.fluid[tag]["correlation"]) + ", &")
        cp = []
        for numbers in self.fluid[tag]["cp"]:
            cpi = '{:.8e}'.format(numbers)
            cp.append(cpi)
        if len(cp) < 21: # Pad list
            cp += ["0.0d0"]*(21-len(cp))
        code_lines.append(3*I + "cp = (/" + cp[0] + "," + cp[1] + "," + cp[2] + "," + cp[3] + "," + cp[4] + ", &")
        code_lines.append(3*I + cp[5] + "," + cp[6] + "," + cp[7] + "," + cp[8] + "," + cp[9] + ", &")
        code_lines.append(3*I + cp[10] + "," + cp[11] + "," + cp[12] + "," + cp[13] + "," + cp[14] + ", &")
        code_lines.append(3*I + cp[15] + "," + cp[16] + "," + cp[17] + "," + cp[18] + "," + cp[19] + "," + cp[20] + "/), &")
        code_lines.append(3*I + 'Tcpmin = {:.4f}'.format(self.fluid[tag]["Tmin"]) + ", &")
        code_lines.append(3*I + 'Tcpmax = {:.4f}'.format(self.fluid[tag]["Tmax"]) + "  &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_TWU_fortran_code(self,eos,TWUtag):
        """
        Output:
        code_lines - Code lines
        """
        d = self.fluid[eos][TWUtag]
        code_lines = []
        code_lines.append(I+"type (alphadatadb), parameter :: TWUTAG = &")
        code_lines.append(3*I+"alphadatadb(eosid=\"" + eos + "\", &")
        code_lines.append(3*I+"cid=\"" + self.fluid["ident"] + "\", &")
        code_lines.append(3*I+"ref=\"" + d["ref"] + "\", &")
        code_lines.append(3*I + 'coeff=(/{:.8e}, {:.8e}, {:.8e}/) &'.format(d["correlation"][0],d["correlation"][1],d["correlation"][2]))
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_VS_fortran_code(self,eos,VStag):
        """
        Output:
        code_lines - Code lines
        """
        d = self.fluid[eos][VStag]
        code_lines = []
        code_lines.append(I + "type (cidatadb), parameter :: VSTAG = &")
        code_lines.append(3*I + "cidatadb(eosid=\"" + eos + "\", &")
        code_lines.append(3*I + "cid=\"" + self.fluid["ident"] + "\", &")
        code_lines.append(3*I + "ref=\"" + d["ref"] + "\", &")
        code_lines.append(3*I + "bib_ref=\"" + d["bib_reference"] + "\", &")
        code_lines.append(3*I + 'ciA={:.8e}, &'.format(d["ciA"]))
        code_lines.append(3*I + 'ciB={:.8e}, &'.format(d["ciB"]))
        code_lines.append(3*I + 'ciC={:.8e}, &'.format(d["ciC"]))
        code_lines.append(3*I + 'c_type={:d} &'.format(d["correlation"]))
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_CPA_fortran_code(self,eos,CPAtag):
        """
        Output:
        code_lines - Code lines
        """
        d = self.fluid[eos][CPAtag]
        code_lines = []
        code_lines.append(I + "type(CPAdata), parameter :: CPATAG = &")
        code_lines.append(3*I + "CPAdata(eosid=\"" + "CPA-"+ eos + "\", &")
        code_lines.append(3*I + "compName=\"" + self.fluid["ident"] + "\", &")
        code_lines.append(3*I + "ref=\"" + d["ref"] + "\", &")
        code_lines.append(3*I + "bib_reference=\"" + d["bib_reference"] + "\", &")
        code_lines.append(3*I + 'a0={:.8e}, &'.format(d["a0"]))
        code_lines.append(3*I + 'b={:.8e}, &'.format(d["b"]))
        code_lines.append(3*I + 'eps={:.8e}, &'.format(d["eps"]))
        code_lines.append(3*I + 'beta={:.8e}, &'.format(d["beta"]))
        code_lines.append(3*I + 'alphacorridx = {}, &'.format(
            get_alpha_index_parameter(d["alphaCorr"])))

        alp = []
        for numbers in d["alphaParams"]:
            alpi = '{:.8e}'.format(numbers)
            alp.append(alpi)
        code_lines.append(3*I + "alphaParams = (/" + alp[0] + "," + alp[1] + "," + alp[2] + "/), &")
        code_lines.append(3*I + 'assoc_scheme = {} &'.format(
            get_assoc_scheme_parameter(d["assoc_scheme"])))
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_MC_fortran_code(self,eos,MCtag):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = self.get_TWU_fortran_code(eos,MCtag)
        code_lines[0] = code_lines[0].replace("TWUTAG", "MCTAG")
        return code_lines

    def get_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """

        # Loop eos-list
        code_lines = self.get_comp_fortran_code()
        for key in self.fluid:
            if "ideal_heat_capacity" in key:
                cp = self.get_cp_fortran_code(key)
                for line in cp:
                    code_lines.append(line)
        for eos in self.eos_list:
            eosDict = self.fluid.get(eos, None)
            if eosDict is not None:
                for key in eosDict.keys():
                    if "TWU" in key:
                        cl = self.get_TWU_fortran_code(eos,key)
                    elif "MC" in key:
                        cl = self.get_MC_fortran_code(eos,key)
                    elif "volume_shift" in key:
                        cl = self.get_VS_fortran_code(eos,key)
                    elif "CPA" in key:
                        cl = self.get_CPA_fortran_code(eos,key)
                    else:
                        "Unknown key ({}) in {} for {}".format(key,self.fluid["ident"],eos)
                    for line in cl:
                        code_lines.append(line)

        return code_lines


class comp_list(object):
    """Read component data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None, comp=component):
        """Load files from data-base
        Arguments:
        filename - path to folder
        """
        # Define counters
        self.nComp = None
        self.nCp = None
        self.nTWU = None
        self.nMC = None
        self.nVS = None
        self.nCPA = None
        if path is None:
            path = "../../../fluids/"
        file_list = os.listdir(path)
        file_list.sort()
        self.comp_list = []
        self.elements = []
        for fl in file_list:
            filepath = os.path.join(path, fl)
            print(fl)
            self.comp_list.append(comp(filepath))
            self.elements += self.comp_list[-1].elements
        self.elements = set(self.elements)
        self.e_ref = entropy_references(self)
        for comp in self.comp_list:
            dHf = float(comp.fluid["reference_state"]["enthalpy"])
            S0 = comp.convert_entropy_reference_state()
            dGf = self.e_ref.calc_gibbs_free_energy_of_formation(comp.elements,
                                                                 dHf, S0)
            comp.dGf = dGf

    def __getitem__(self, fluid):
        """
        Arguments:
        fluid (str): Fluid name
        """
        k = None
        for i in range(len(self.comp_list)):
            if self.comp_list[i].fluid["ident"] == fluid:
                k = i
                break
        if k is None:
            raise ValueError("Fluid not in list: " + fluid)
        return self.comp_list[k]

    def save_fortran_file(self,filename):
        """Generate fortran file for Thermopack
        Arguments:
        filename - path to folder
        """
        code_lines = []
        code_lines.append("!> Automatically generated to file compdatadb.f90")
        code_lines.append("!! using utility python code pyUtils")
        now = datetime.today().isoformat()
        code_lines.append("!! Time stamp: " + now)
        code_lines.append("")
        code_lines.append("module compdatadb")
        code_lines.append(I+"use compdata, only: gendatadb, cpdata, alphadatadb, cidatadb, CPAdata")
        code_lines.append(I+"use assocschemeutils")
        code_lines.append(I+"use cubic_eos")
        code_lines.append(I+"use thermopack_constants, only: element_len")
        code_lines.append(I+"implicit none")
        code_lines.append(I+"public")
        code_lines.append("")

        element_list = self.e_ref.get_element_list()
        code_lines.append(I+f"integer, parameter :: n_max_elements = {str(len(element_list))}")
        code_lines.append(I+"character(len=element_len), dimension(n_max_elements), parameter :: elements =(/&")
        elements = ""
        for ie, e in enumerate(element_list):
            elements += f"\"{e.ljust(2, ' ')}\", "
        elements = elements[:-2] + "/)"
        code_lines.append(2*I+elements)
        code_lines.append("")

        self.nComp = 1
        self.nCp = 1
        self.nTWU = 1
        self.nMC = 1
        self.nVS = 1
        self.nCPA = 1
        for comp in self.comp_list:
            comp_code_lines = comp.get_fortran_code()
            for il, line in enumerate(comp_code_lines):
                new_line = line
                new_line = self.replace_comp_tag(new_line)
                new_line = self.replace_cp_tag(new_line)
                new_line = self.replace_TWU_tag(new_line)
                new_line = self.replace_MC_tag(new_line)
                new_line = self.replace_VS_tag(new_line)
                new_line = self.replace_CPA_tag(new_line)
                code_lines.append(new_line)
        # Correct count to actual number of entries
        self.nComp -= 1
        self.nCp -= 1
        self.nTWU -= 1
        self.nMC -= 1
        self.nVS -= 1
        self.nCPA -= 1

        cl = self.get_comp_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_cp_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_twu_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_mc_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_vs_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        cl = self.get_cpa_array_fortran_code()
        for line in cl:
            code_lines.append(line)

        code_lines.append("")
        code_lines.append("end module compdatadb")

        new_contents = '\n'.join(code_lines)
        tools.write_file(f'{tools.THERMOPACK_ROOT}/src/{filename}', new_contents)

    def get_comp_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxncdb =" + str(self.nComp))
        code_lines.append(I+"type (gendatadb), dimension(maxncdb), parameter :: compdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nComp,self.get_comp_tag)
        return code_lines

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

    def get_cp_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxcpdb =" + str(self.nCp))
        code_lines.append(I+"type (cpdata), dimension(maxcpdb), parameter :: cpdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nCp,self.get_cp_tag)
        return code_lines

    def get_vs_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxcidb =" + str(self.nVS))
        code_lines.append(I+"type (cidatadb), dimension(maxcidb), parameter :: cidb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nVS,self.get_VS_tag)
        return code_lines

    def get_twu_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxTWUdb =" + str(self.nTWU))
        code_lines.append(I+"type (alphadatadb), dimension(maxTWUdb), parameter :: alphaTWUdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nTWU,self.get_TWU_tag)
        return code_lines

    def get_mc_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: maxMCdb =" + str(self.nMC))
        code_lines.append(I+"type (alphadatadb), dimension(maxMCdb), parameter :: alphaMCdb = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nMC,self.get_MC_tag)
        return code_lines

    def get_cpa_array_fortran_code(self):
        """Set up component array
        Output:
        code_lines - Code lines
        """
        code_lines = [""]
        code_lines.append(I+"integer, parameter :: nCPAmodels =" + str(self.nCPA))
        code_lines.append(I+"type(CPAdata), dimension(nCPAmodels), parameter :: CPAarray = (/&")
        code_lines = self.get_array_fortran_code(code_lines,self.nCPA,self.get_CPA_tag)
        return code_lines

    def get_comp_tag(self,i_comp=None):
        """FORTRAN component parameter name
        """
        comptag = "cx"
        if i_comp is None:
            comptag += str(self.nComp)
        else:
            comptag += str(i_comp)
        return comptag

    def replace_comp_tag(self, line):
        """Replace COMPTAG with FORTRAN component parameter name
        """
        comptag = self.get_comp_tag()
        if "COMPTAG" in line:
            line = line.replace("COMPTAG",comptag)
            self.nComp += 1
        return line

    def get_cp_tag(self,i_cp=None):
        """FORTRAN Cp parameter name
        """
        cptag = "cp"
        if i_cp is None:
            cptag += str(self.nCp)
        else:
            cptag += str(i_cp)
        return cptag

    def replace_cp_tag(self, line):
        """Replace CPTAG with FORTRAN Cp parameter name
        """
        cptag = self.get_cp_tag()
        if "CPTAG" in line:
            line = line.replace("CPTAG",cptag)
            self.nCp += 1
        return line

    def get_TWU_tag(self,i_twu=None):
        """FORTRAN TWU parameter name
        """
        twutag = "twu"
        if i_twu is None:
            twutag += str(self.nTWU)
        else:
            twutag += str(i_twu)
        return twutag

    def replace_TWU_tag(self, line):
        """Replace TWUTAG with FORTRAN TWU parameter name
        """
        twutag = self.get_TWU_tag()
        if "TWUTAG" in line:
            line = line.replace("TWUTAG",twutag)
            self.nTWU += 1
        return line

    def get_MC_tag(self,i_mc=None):
        """FORTRAN MC parameter name
        """
        mctag = "mc"
        if i_mc is None:
            mctag += str(self.nMC)
        else:
            mctag += str(i_mc)
        return mctag

    def replace_MC_tag(self, line):
        """Replace MCTAG with FORTRAN MC parameter name
        """
        mctag = self.get_MC_tag()
        if "MCTAG" in line:
            line = line.replace("MCTAG",mctag)
            self.nMC += 1
        return line

    def get_VS_tag(self,i_vs=None):
        """FORTRAN VS parameter name
        """
        vstag = "c"
        if i_vs is None:
            vstag += str(self.nVS)
        else:
            vstag += str(i_vs)
        return vstag

    def replace_VS_tag(self, line):
        """Replace VSTAG with FORTRAN VS parameter name
        """
        vstag = self.get_VS_tag()
        if "VSTAG" in line:
            line = line.replace("VSTAG",vstag)
            self.nVS += 1
        return line


    def save_wiki_name_mapping(self,filename):
        """Generate table with name to comp. id. mapping
        Arguments:
        filename - path to file
        """
        wiki_header_lines = []
        wiki_header_lines.append('---\n'
                                'version: \n'
                                'layout: default\n'
                                'title: Component identifiers\n'
                                'permalink: /vcurrent/Component-name-mapping.html\n'
                                '---\n\n')
        wiki_header_lines.append('<!---\nThis is an auto-generated file, written by the module at '
                                 'addon/pyUtils/compdatadb.py\n'
                                 'Generated at : ' + datetime.today().isoformat() + '\n'
                                 'This is the same module that is used to generate the Fortran\n'
                                 'component database files.\n'
                                 '--->\n\n')
        wiki_header_lines.append("# Fluid name to fluid identifier mapping")
        wiki_header_lines.append("&nbsp;\n")
        wiki_header_lines.append("In order to specify fluids in Thermopack you need to use fluid identifiers as shown in the table below. The 'SAFT-VR', 'PC-SAFT' and 'CPA' columns indicate which fluids SAFT-EoS and CPA parameters are available for.\n")
        wiki_header_lines.append("&nbsp;\nYou may have to scroll right to view the whole table.\n")
        wiki_header_lines.append("| Fluid name | CAS Number |Fluid identifyer | SAFT-VR | PC-SAFT | CPA |")
        wiki_header_lines.append("| ------------------------ | ---- | ----------- | ---- | ---- | ---- |")

        wiki_lines = []
        for comp in self.comp_list:
            name = comp.fluid["name"].lower()
            if name[:2] in ("n-", "m-", "o-", "p-"):
                name = name[:2] + name[2:].capitalize()
            elif name[0].isdigit():
                for i in range(1,len(name)):
                    if name[i].isalpha():
                        name = name[:i] + name[i:].capitalize()
                        break
            else:
                name = name.capitalize()

            has_svrm_params = False
            has_pcsaft_params = False
            has_cpa_params = False
            for k in comp.fluid.keys():
                if 'saftvrmie' in k.lower():
                    has_svrm_params = True
                elif 'pc-saft' in k.lower():
                    has_pcsaft_params = True
                elif ('srk' in k.lower()) or ('pr' in k.lower()):
                    for sub_k in comp.fluid[k].keys():
                        if 'cpa' in sub_k.lower():
                            has_cpa_params = True
                if has_svrm_params and has_cpa_params and has_pcsaft_params:
                    break

            def has_param_txt(has_param):
                if has_param is True:
                    return '&#10004;'
                return ' '

            has_svrm_params = has_param_txt(has_svrm_params)
            has_pcsaft_params = has_param_txt(has_pcsaft_params)
            has_cpa_params = has_param_txt(has_cpa_params)

            line = f"| {name} | {comp.fluid['cas_number']} | {comp.fluid['ident']} | {has_svrm_params} | {has_pcsaft_params}" \
                   f" | {has_cpa_params} |"
            wiki_lines.append(line)
        wiki_lines.sort()
        wiki_lines = wiki_header_lines + wiki_lines
        new_filestr = ''
        for line in wiki_lines:
            new_filestr += line + '\n'

        if tools.check_is_changed(filename, new_filestr):
            with open(filename, "w") as f:
                f.write(new_filestr)
            print(f'Updated file : {filename}')
        else:
            print(f'File at {filename} is unchanged.')



    def get_CPA_tag(self,i_cpa=None):
        """FORTRAN CPA parameter name
        """
        cpatag = "cpa"
        if i_cpa is None:
            cpatag += str(self.nCPA)
        else:
            cpatag += str(i_cpa)
        return cpatag

    def replace_CPA_tag(self, line):
        """Replace CPATAG with FORTRAN CPA parameter name
        """
        cpatag = self.get_CPA_tag()
        if "CPATAG" in line:
            line = line.replace("CPATAG",cpatag)
            self.nCPA += 1
        return line


if __name__ == "__main__":
    compl = comp_list()
    compl.save_fortran_file("compdatadb.f90")
    compl.save_wiki_name_mapping(tools.MARKDOWN_DIR + "Component-name-mapping.md")
