from PyQt5.QtWidgets import QMessageBox
from PyQt5.QtGui import QValidator

from thermopack.cubic import cubic
from thermopack.cpa import cpa
from thermopack.pcsaft import pcsaft
from thermopack.saftvrmie import saftvrmie

import json
import os
import re

from pathlib import Path
import numpy as np

APP_ROOT = Path(os.path.dirname(__file__))

class Component:
    """
    Data structure for a component. Contains a name, and the data from the fluids folder
    """

    def __init__(self, name, json_data):
        self.name = name
        self.json_data = json_data

    def __str__(self):
        return self.name


class MessageBox(QMessageBox):
    """
    Popup to display important information to the user
    """

    def __init__(self, window_title, message):
        QMessageBox.__init__(self)
        self.setWindowTitle(window_title)
        self.setText(message)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)


class FloatValidator(QValidator):
    _float_re = re.compile(r'(([+-]?\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?)')

    def valid_float_string(self, string):
        match = self._float_re.search(string)
        if match:
            return match.groups()[0] == string
        else:
            return False

    def validate(self, string, position):
        if self.valid_float_string(string):
            state = QValidator.Acceptable
        elif string == "" or string[position - 1] in 'e.-+' and string.lower() != "e":
            state = QValidator.Intermediate
        else:
            state = QValidator.Invalid
        return state, string, position

    def fixup(self, text):
        match = self._float_re.search(text)
        return match.groups()[0] if match else "0.0"


def get_default_units():
    return {
        "Energy": "J",
        "Temperature": "degK",
        "Pressure": "Pa",
        "Volume": "m ** 3",
        "Amount": "mol",
        "Speed": "m / s"
    }


def get_json_data(file_name):
    """
    Opens a JSON-file and returns a Python dictionary containing the file data
    :param file_name: Name of file to be opened
    :return: Python-dict
    """
    with open(file_name) as file:
        return json.load(file)


def save_json_data(new_data, file_name):
    """
    Opens a JSON-file, and saves the data to it. If there already is some data in the file, it is updated
    :param new_data: Data to be saved
    :param file_name: Name of file
    """
    if os.path.exists(file_name):
        data = get_json_data(file_name)
        data.update(new_data)
    else:
        data = new_data

    with open(file_name, "w") as file:
        json.dump(data, file, sort_keys=True, indent=4, separators=(',', ': '))


def get_unique_name(prefix, lst):
    """
    Appending numbers to a prefix until a unique name is obtained, which is not contained in the passed list.
    If the number gets over 100, the function breaks and does not return anything
    :param prefix: Prefix for the uniqe name
    :param lst: List to which the name is compared
    :return: Uniqe name
    """
    i = 1
    while True:
        name = prefix + " " + str(i)

        if i >= 100:
            break

        elif name not in lst:
            return name

        else:
            i += 1


def get_fluids():
    """
    Opens the fluids folder, and returns a dict containing the data for the components
    :return: fluids dict
    """
    fluids = {}

    fluids_path = os.path.join(APP_ROOT, "fluids")

    for root, dirs, files in os.walk(fluids_path):
        for file_name in files:
            file = open(os.path.join(root, file_name), "r")

            component_data = json.load(file)
            name = component_data["name"]

            component = Component(name, component_data)
            fluids[name] = component

    return fluids


def get_comp_id(comp_list_data, comp_name):
    """
    Returns the identity (which is used by thermopack) corresponding to a given component name
    :param comp_list_data: Session component list data (dict)
    :param comp_name: Name of component of which the identity is desired
    :return: Component identity
    """
    index = comp_list_data["Names"].index(comp_name)
    return comp_list_data["Identities"][index]


def get_thermopack(category):
    """
    Returns the correct type of thermopack instance depending on the chosen model
    :param category: Name of model category
    :return: Thermopack instance
    """
    if category == "Cubic":
        return cubic()
    elif category == "CPA":
        return cpa()
    elif category == "PC-SAFT":
        return pcsaft()
    elif category == "SAFT-VR Mie":
        return saftvrmie()
    else:
        return None


def init_thermopack(tp, comp_data, comp_list_name, settings):
    """
    Initiates thermopack with the selected model options and interaction parameters
    :param tp: Thermopack instance to be initiated
    :param comp_data: dict, data of the current composition
    :param comp_list_name: str, name of current composition
    :param settings: dict, data of the selected model
    """
    comp_list = comp_data["Identities"]
    comps = ",".join(comp_list)
    model_ref = settings["Model options"]["Reference"]

    if comp_list_name in settings["Parameters"].keys():
        parameters_exist = (len(settings["Parameters"][comp_list_name].keys()) > 0)
        if parameters_exist:
            all_matrices = settings["Parameters"][comp_list_name]["Coefficient matrices"]
    else:
        parameters_exist = False

    category = settings["Model category"]
    if category == "Cubic":

        eos = settings["EOS"]
        mixing = settings["Model options"]["Mixing rule"]
        alpha = settings["Model options"]["Alpha correlation"]

        tp.init(comps=comps, eos=eos, mixing=mixing, alpha=alpha, parameter_reference=model_ref)

        if parameters_exist:
            if mixing == "vdW" and "VDW K" in all_matrices.keys():
                matrix = all_matrices["VDW K"]

                for row in range(len(matrix)):
                    for col in range(len(matrix)):

                        c1 = comp_list[row]
                        c2 = comp_list[col]
                        index1 = tp.getcompindex(c1)
                        index2 = tp.getcompindex(c2)
                        if row != col:
                            tp.set_kij(index1, index2, matrix[row][col])

            elif mixing in ["HV1", "HV2"]:

                if "HV1 Alpha" in all_matrices.keys():
                    alpha_matrix = all_matrices["HV1 Alpha"]
                    a_matrix = all_matrices["HV1 A"]
                    b_matrix = all_matrices["HV1 B"]
                    c_matrix = all_matrices["HV1 C"]

                elif "HV2 Alpha" in all_matrices.keys():
                    alpha_matrix = all_matrices["HV2 Alpha"]
                    a_matrix = all_matrices["HV2 A"]
                    b_matrix = all_matrices["HV2 B"]
                    c_matrix = all_matrices["HV2 C"]

                else:
                    return

                for row in range(len(a_matrix)):
                    for col in range(len(a_matrix)):

                        c1 = comp_list[row]
                        c2 = comp_list[col]
                        index1 = tp.getcompindex(c1)
                        index2 = tp.getcompindex(c2)

                        if row != col and row < col:
                            alpha_ij = alpha_matrix[row][col]
                            alpha_ji = alpha_matrix[col][row]
                            a_ij = a_matrix[row][col]
                            a_ji = a_matrix[col][row]
                            b_ij = b_matrix[row][col]
                            b_ji = b_matrix[col][row]
                            c_ij = c_matrix[row][col]
                            c_ji = c_matrix[col][row]

                            tp.set_hv_param(index1, index2, alpha_ij, alpha_ji,
                                            a_ij, a_ji, b_ij, b_ji, c_ij, c_ji)

    elif category == "CPA":
        eos = settings["EOS"]
        mixing = settings["Model options"]["Mixing rule"]
        alpha = settings["Model options"]["Alpha correlation"]
        tp.init(comps, eos=eos, mixing=mixing, alpha=alpha, parameter_reference=model_ref)

        if parameters_exist and "CPA K" in all_matrices.keys():
            k_matrix = comp_data["Coefficient matrices"]["CPA K"]
            eps_matrix = comp_data["Coefficient matrices"]["CPA Epsilon"]

            for row in range(len(k_matrix)):
                for col in range(len(k_matrix)):

                    c1 = comp_list[row]
                    c2 = comp_list[col]
                    index1 = tp.getcompindex(c1)
                    index2 = tp.getcompindex(c2)

                    if row != col:
                        k_ij = k_matrix[row][col]
                        eps_kij = eps_matrix[row][col]
                        tp.set_kij(index1, index2, np.array([k_ij, eps_kij]))

    elif category == "PC-SAFT":
        tp.init(comps=comps, parameter_reference=model_ref)

        if parameters_exist and "PC-SAFT K" in all_matrices.keys():
            matrix = comp_data["Coefficient matrices"]["K"]

            for row in range(len(matrix)):
                for col in range(len(matrix)):

                    c1 = comp_list[row]
                    c2 = comp_list[col]
                    index1 = tp.getcompindex(c1)
                    index2 = tp.getcompindex(c2)

                    if row != col:
                        tp.set_kij(index1, index2, matrix[row][col])

    elif category == "SAFT-VR Mie":
        a1 = settings["Model options"]["A1"]
        a2 = settings["Model options"]["A2"]
        a3 = settings["Model options"]["A3"]
        hard_sphere = settings["Model options"]["Hard sphere"]
        chain = settings["Model options"]["Chain"]

        tp.model_control_a1(a1)
        tp.model_control_a2(a2)
        tp.model_control_a3(a3)
        tp.model_control_hard_sphere(hard_sphere)
        tp.model_control_chain(chain)

        tp.init(comps=comps, parameter_reference=model_ref)

        if parameters_exist:
            pure_fluid_parameters_exist = (
                    len(settings["Parameters"][comp_list_name]["Pure fluid parameters"].keys()) > 0
            )

            if "SAFT-VR Mie Epsilon" in all_matrices.keys():
                epsilon_matrix = all_matrices["SAFT-VR Mie Epsilon"]
                sigma_matrix = all_matrices["SAFT-VR Mie Sigma"]
                gamma_matrix = all_matrices["SAFT-VR Mie Gamma"]

                for row in range(len(epsilon_matrix)):
                    for col in range(len(epsilon_matrix)):

                        c1 = comp_list[row]
                        c2 = comp_list[col]
                        index1 = tp.getcompindex(c1)
                        index2 = tp.getcompindex(c2)
                        if row != col:
                            tp.set_eps_kij(index1, index2, epsilon_matrix[row][col])
                            tp.set_sigma_lij(index1, index2, sigma_matrix[row][col])
                            tp.set_lr_gammaij(index1, index2, gamma_matrix[row][col])

            if pure_fluid_parameters_exist:
                pure_fluid_parameters = settings["Parameters"][comp_list_name]["Pure fluid parameters"]

                for comp_id in comp_data["Identities"]:
                    comp_index = tp.getcompindex(comp_id)

                    if None not in pure_fluid_parameters[comp_id].values():
                        m = pure_fluid_parameters[comp_id]["M"]
                        sigma = pure_fluid_parameters[comp_id]["Sigma"]
                        eps_div_k = pure_fluid_parameters[comp_id]["Epsilon"]
                        lambda_a = pure_fluid_parameters[comp_id]["Lambda a"]
                        lambda_r = pure_fluid_parameters[comp_id]["Lambda r"]

                        tp.set_pure_fluid_param(comp_index, m, sigma, eps_div_k, lambda_a, lambda_r)
