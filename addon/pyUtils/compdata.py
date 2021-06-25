"""Module for calculating Mie potential."""
from __future__ import print_function
import numpy as np
from sys import exit
import os
import math
import json
from datetime import datetime

I = "  "
N_TAGS_PER_LINE = 5

def get_keys_from_base_name(d,name):
    key_list = []
    for key in d.keys():
        if name in key:
            key_list.append(key)
    return key_list

class component(object):
    """Read component data form file, manipulate, save and generate
    component data file for Thermopack
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        self.eos_list = ["PR", "SRK", "CSP-SRK", "CSP-PR",
                         "CPA-SRK", "CPA-SRK", "LK", "PC-SAFT"
                         "SAFT-VR-MIE", "RK", "VDW", "SW", "PT"]
        self.filepath = filepath
        with open(filepath) as f:
            self.comp = json.load(f)

    def save(self):
        """Save to file
        """
        with open(self.filepath, "w") as f:
            json.dump(self.comp,f,indent=2)

    def get_n_TWU(self):
        """Set Mie potential paramaters
        Input:
        eos - String defining EOS
        Output:
        n_TWU - Number of TWU entries
        """
        n_TWU = 0
        for eos in self.eos_list:
            d = self.comp[eos]
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
            d = self.comp[eos]
            n_MC += len(get_keys_from_base_name(d,"MC"))
        return n_MC

    def get_comp_fortran_code(self):
        """
        Output:
        code_lines - Code lines
        """
        code_lines = []
        code_lines.append(I+"type (gendatadb), parameter :: COMPTAG = &")
        code_lines.append(3*I+"gendatadb(ident = \"" + self.comp["ident"] + "\", &")
        code_lines.append(3*I+"formula = \"" + self.comp["formula"] + "\", &")
        code_lines.append(3*I+"name = \"" + self.comp["name"] + "\", &")
        #code_lines.append(3*I+"\"ref=" + self.comp["ref"] + "\", &")
        code_lines.append(3*I + 'mw = {:.4f}'.format(self.comp["mol_weight"]) + ", &")
        code_lines.append(3*I + 'Tc = {:.4f}'.format(self.comp["critical"]["temperature"]) + ", &")
        code_lines.append(3*I + 'Pc = {:.2f}'.format(self.comp["critical"]["pressure"]) + ", &")
        code_lines.append(3*I + 'Zc = {:.6f}'.format(self.comp["critical"]["compressibility"]) + ", &")
        code_lines.append(3*I + 'acf = {:.6f}'.format(self.comp["acentric_factor"]["acf"]) + ", &")
        code_lines.append(3*I + 'Tb = {:.4f}'.format(self.comp["boiling_temperature"]["temperature"]) + ", &")
        code_lines.append(3*I + 'Ttr = {:.4f}'.format(self.comp["triple"]["temperature"]) + ", &")
        code_lines.append(3*I + 'Ptr = {:.4f}'.format(self.comp["triple"]["pressure"]) + ", &")
        code_lines.append(3*I + 'sref = {:.4f}'.format(self.comp["reference_state"]["entropy"]) + ", &")
        code_lines.append(3*I + 'href = {:.4f}'.format(self.comp["reference_state"]["enthalpy"]) + ", &")
        code_lines.append(3*I + 'DfH = {:.4f}'.format(self.comp["standard_formation_energy"]["enthalpy"]) + ", &")
        code_lines.append(3*I + 'DfG = {:.4f}'.format(self.comp["standard_formation_energy"]["gibbs"]) + ", &")
        code_lines.append(3*I + "psatcode = " + str(self.comp["saturation"]["correlation"]) + ", &")
        code_lines.append(3*I + 'ant = (/{:.8e}, {:.8e}, {:.8e}/)'.format(self.comp["saturation"]["ant"][0], self.comp["saturation"]["ant"][1], self.comp["saturation"]["ant"][2]) + ", &")
        code_lines.append(3*I + 'Tantmin = {:.4f}'.format(self.comp["saturation"]["Tmin"]) + ", &")
        code_lines.append(3*I + 'Tantmax = {:.4f}'.format(self.comp["saturation"]["Tmax"]) + ", &")
        code_lines.append(3*I + 'Zra = {:.6f}'.format(self.comp["rackett_factor"]["Zra"]) + " &")
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
        code_lines.append(3*I+"cpdata(cid = \"" + self.comp["ident"] + "\", &")
        code_lines.append(3*I+"ref = \"" + self.comp[tag]["ref"] + "\", &")
        code_lines.append(3*I+"bib_ref = \"" + self.comp[tag]["bib_reference"] + "\", &")
        code_lines.append(3*I + "cptype = " + str(self.comp[tag]["correlation"]) + ", &")
        cp = []
        for numbers in self.comp[tag]["cp"]:
            cpi = '{:.8e}'.format(numbers)
            cp.append(cpi)
        code_lines.append(3*I + "cp = (/" + cp[0] + "," + cp[1] + "," + cp[2] + "," + cp[3] + "," + cp[4] + ", &")
        code_lines.append(3*I + cp[5] + "," + cp[6] + "," + cp[7] + "," + cp[8] + "," + cp[9] + "/), &")
        code_lines.append(3*I + 'Tcpmin = {:.4f}'.format(self.comp[tag]["Tmin"]) + ", &")
        code_lines.append(3*I + 'Tcpmax = {:.4f}'.format(self.comp[tag]["Tmax"]) + "  &")
        code_lines.append(3*I + ")")
        code_lines.append("")

        return code_lines

    def get_TWU_fortran_code(self,eos,TWUtag):
        """
        Output:
        code_lines - Code lines
        """
        d = self.comp[eos][TWUtag]
        code_lines = []
        code_lines.append(I+"type (alphadatadb), parameter :: TWUTAG = &")
        code_lines.append(3*I+"alphadatadb(eosid=\"" + eos + "\", &")
        code_lines.append(3*I+"cid=\"" + self.comp["ident"] + "\", &")
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
        d = self.comp[eos][VStag]
        code_lines = []
        code_lines.append(I + "type (cidatadb), parameter :: VSTAG = &")
        code_lines.append(3*I + "cidatadb(eosid=\"" + eos + "\", &")
        code_lines.append(3*I + "cid=\"" + self.comp["ident"] + "\", &")
        code_lines.append(3*I + "ref=\"" + d["ref"] + "\", &")
        code_lines.append(3*I + "bib_ref=\"" + d["bib_reference"] + "\", &")
        code_lines.append(3*I + 'ciA={:.8e}, &'.format(d["ciA"]))
        code_lines.append(3*I + 'ciB={:.8e}, &'.format(d["ciB"]))
        code_lines.append(3*I + 'ciC={:.8e}, &'.format(d["ciC"]))
        code_lines.append(3*I + 'c_type={:d} &'.format(d["correlation"]))
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
        for key in self.comp:
            if "ideal_heat_capacity" in key:
                cp = self.get_cp_fortran_code(key)
                for line in cp:
                    code_lines.append(line)
        for eos in self.eos_list:
            eosDict = self.comp.get(eos, None)
            if eosDict is not None:
                for key in eosDict.keys():
                    if "TWU" in key:
                        cl = self.get_TWU_fortran_code(eos,key)
                    elif "MC" in key:
                        cl = self.get_MC_fortran_code(eos,key)
                    elif "volume_shift" in key:
                        cl = self.get_VS_fortran_code(eos,key)
                    else:
                        "Unknown key ({}) in {} for {}".format(key,self.comp["ident"],eos)
                    for line in cl:
                        code_lines.append(line)

        return code_lines

class comp_list(object):
    """Read component data files into list save and generate
    component data file for Thermopack
    """

    def __init__(self, path=None):
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
        if path is None:
            file_list = os.listdir("../../fluids/")
        else:
            file_list = os.listdir(path)
        file_list.sort()
        self.comp_list = []
        for fl in file_list:
            filepath = os.path.join("../../fluids/", fl)
            print(fl)
            self.comp_list.append(component(filepath))

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
        code_lines.append(I+"use compdata, only: gendatadb, cpdata, alphadatadb, cidatadb")
        code_lines.append(I+"implicit none")
        code_lines.append(I+"public")
        code_lines.append("")

        self.nComp = 1
        self.nCp = 1
        self.nTWU = 1
        self.nMC = 1
        self.nVS = 1
        for comp in self.comp_list:
            comp_code_lines = comp.get_fortran_code()
            for il, line in enumerate(comp_code_lines):
                new_line = line
                new_line = self.replace_comp_tag(new_line)
                new_line = self.replace_cp_tag(new_line)
                new_line = self.replace_TWU_tag(new_line)
                new_line = self.replace_MC_tag(new_line)
                new_line = self.replace_VS_tag(new_line)
                code_lines.append(new_line)
        # Correct count to actual number of entries
        self.nComp -= 1
        self.nCp -= 1
        self.nTWU -= 1
        self.nMC -= 1
        self.nVS -= 1

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

        code_lines.append("")
        code_lines.append("end module compdatadb")

        with open(filename, "w") as f:
            for line in code_lines:
                f.write(line)
                f.write("\n")

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

