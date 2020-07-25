from PyQt5.QtWidgets import QWidget, QTableWidget, QTableWidgetItem, QHeaderView, QTabWidget, QRadioButton, QButtonGroup
from PyQt5.uic import loadUi
from PyQt5 import QtCore

from cubic import cubic
from cpa import cpa
from pcsaft import pcsaft
from saftvrmie import saftvrmie

from gui.utils import get_comp_id

import numpy as np


# TODO: Ikke sikkert at disse trenger en self.thermopack. Koeffisientene kan fint regnes ut uten å instansiere den.
#  Foreløpig må den være der ettersom jeg ikke har en annen måte å få tak i dem på

# TODO: Når fanen byttes skal lista deselectes og table_stack --> page0 (tom)

# TODO: Methane + CO2 + CPA --> error når jeg switcher til bin coeff tab

# TODO: Lage en bedre klassestruktur med arv. Veeldig mye likt i disse nå

# TODO: Med flere komponentlister og flere modellister, kan det hende at en komponentliste har flere forskjellige
#  matriser av samme type. (F eks én standard og én med endra interaksjonsparametere)
#  Må ha List name { Settings name { Coeff matrices { vdW K: [[]] }, Identities: [], Names: [] } }

# TODO: Må endre navn på matriser. Se i model_select_widget.py

# TODO: PC-SAFT skal være symmetrisk likevel

# TODO: Når ting endres i matrisa: Trenger ikke endre i termopack. Den må endres etterpå uansett. Trenger bare
#  vise i tabellen og lagre i json-data. Da må vi passe på at hvis dataen skal hentes ut igjen, ta det fra json, ikke fra tp


class ParametersWidget(QWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(parent=parent)

        self.settings = data["Model setups"][settings_name]
        self.component_lists = data["Component lists"]

    def get_thermopack(self):
        category = self.settings["Model category"]
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

    def init_thermopack(self, list_name):
        # This must be run before coefficients are calculated
        comps = ",".join(self.component_lists[list_name]["Identities"])
        ref = self.settings["Model options"]["Reference"]

        if isinstance(self.thermopack, cubic) or isinstance(self.thermopack, cpa):
            eos = self.settings["EOS"]
            mixing = self.settings["Model options"]["Mixing rule"]
            alpha = self.settings["Model options"]["Alpha correlation"]

            if mixing == "HV1":
                # To give thermopack the correct parameter
                mixing = "HV"

            self.thermopack.init(comps, eos, mixing=mixing, alpha=alpha, parameter_reference=ref)

        elif isinstance(self.thermopack, pcsaft):
            self.thermopack.init(comps, parameter_reference=ref)

        elif isinstance(self.thermopack, saftvrmie):
            self.thermopack.init(comps, parameter_reference=ref)

    def init_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)

    def get_table(self, list_name, table_name):
        """
        :param list_name: str, name of component list to which the table is corresponsing
        :param table_name: str, name of table (could be K, alpha, A, B, C. epsilon, sigma, gamma depending on mixing rule)
        :return: QTableWidget, table containing the correct parameters
        """
        composition = self.component_lists[list_name]["Names"]
        matrix_data = self.component_lists[list_name]["Coefficient matrices"][table_name]
        size = len(composition)

        table = QTableWidget(size, size)

        # Insert coefficients into table
        for row in range(size):
            table.setVerticalHeaderItem(row, QTableWidgetItem(composition[row]))

        for col in range(size):
            table.setHorizontalHeaderItem(col, QTableWidgetItem(composition[col]))

        for i in range(size):
            for j in range(size):

                if i == j:
                    item = QTableWidgetItem("-")
                    # Not editable
                    item.setFlags(QtCore.Qt.NoItemFlags)

                else:
                    item = QTableWidgetItem(str(matrix_data[i][j]))

                item.setTextAlignment(QtCore.Qt.AlignCenter)

                table.blockSignals(True)
                table.setItem(i, j, item)
                table.blockSignals(False)

                if table_name in ["K", "epsilon", "sigma", "gamma"]:
                    # Matrix is symmetric, so these items can't be edited
                    if i >= j:
                        item.setFlags(QtCore.Qt.NoItemFlags)

        header = table.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeToContents)

        return table


class VdWBinaryCoefficientsWidget(ParametersWidget):
    # Should be reinitiated every time opened, so that coefficients are correct if model is changed
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/vdw_bin_coeff_widget.ui", self)

        self.composition_list.currentItemChanged.connect(self.show_correct_matrix)
        self.stack_indices = {}

    def init_widget(self, data, settings_name):
        self.settings = data["Model setups"][settings_name]

        self.thermopack = self.get_thermopack()

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for name in self.component_lists.keys():
            if name not in list_names:

                if "Coefficient matrices" not in self.component_lists[name].keys():
                    self.component_lists[name]["Coefficient matrices"] = {}

                # Create one table for each list in a stacked widget.
                # The dict keeps track of the table's index in the stack

                # Create table
                self.calculate_matrix_data(name)
                table = self.get_table(name, "K")
                table.itemChanged.connect(lambda item: self.change_coeff(item, table, "K"))

                # Keep track of table in stack
                index = self.table_stack.addWidget(table)
                self.stack_indices[name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name, reset=False):
        """
        :param list_name: str, Name of component list
        :param reset: bool, If True, existing matrix data will be overloaded by thermopack's default values
        Calculates binary coefficients for all composition lists and stores them
        """
        self.init_thermopack(list_name)

        if "K" in self.component_lists[list_name]["Coefficient matrices"].keys() and not reset:
            # Loaded data should not be replaced
            return

        # Creating 2D array to be stored in self.data
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        matrix_data = np.zeros(shape=(size, size))

        for row in range(size):
            for col in range(size):
                identity1 = self.component_lists[list_name]["Identities"][row]
                identity2 = self.component_lists[list_name]["Identities"][col]
                index1 = self.thermopack.getcompindex(identity1)
                index2 = self.thermopack.getcompindex(identity2)
                coeff = self.thermopack.get_kij(index1, index2)
                matrix_data[row][col] = coeff

        for row in range(size):
            matrix_data[row][row] = 0.0

        matrix_data = matrix_data.tolist()
        self.component_lists[list_name]["Coefficient matrices"]["K"] = matrix_data

    def show_correct_matrix(self, list_item):
        if not list_item:
            return
        list_name = list_item.text()
        index = self.stack_indices[list_name]
        self.table_stack.setCurrentIndex(index)

    def change_coeff(self, item, table, table_name):
        # Item has to be changed in table and self.data
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.component_lists[component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value

        matrix[col][row] = value
        table.blockSignals(True)
        table.item(col, row).setText(str(value))
        table.blockSignals(False)


class HV1BinaryCoefficientsWidget(ParametersWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/hv1_bin_coeff_widget.ui", self)
        self.tab_stack_indices = {}

        self.composition_list.currentItemChanged.connect(self.show_correct_tab_widget)

    def init_widget(self, data, settings_name):
        self.settings = data["Model setups"][settings_name]
        self.thermopack = self.get_thermopack()

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for list_name in self.component_lists.keys():
            if list_name not in list_names:
                self.component_lists[list_name]["Coefficient matrices"] = {}

                # Create one tab for each list in a tab widget. The dict keeps track of the tab's index
                self.calculate_matrix_data(list_name)

                self.alpha_table = self.get_table(list_name, "alpha")
                self.a_table = self.get_table(list_name, "A")
                self.b_table = self.get_table(list_name, "B")
                self.c_table = self.get_table(list_name, "C")

                self.alpha_table.itemChanged.connect(lambda item: self.change_coeff(item, "alpha"))
                self.a_table.itemChanged.connect(lambda item: self.change_coeff(item, "A"))
                self.b_table.itemChanged.connect(lambda item: self.change_coeff(item, "B"))
                self.c_table.itemChanged.connect(lambda item: self.change_coeff(item, "C"))

                tab_widget = HV1TabWidget(self.alpha_table, self.a_table, self.b_table)

                # Keep track of tabs in stack
                index = self.tab_stack.addWidget(tab_widget)
                self.tab_stack_indices[list_name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name):
        # Creating 2D arrays to be stored in self.data
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        alpha_matrix_data = np.zeros(shape=(size, size))
        a_matrix_data = np.zeros(shape=(size, size))
        b_matrix_data = np.zeros(shape=(size, size))
        c_matrix_data = np.zeros(shape=(size, size))

        self.init_thermopack(list_name)

        for row in range(size - 1):
            for col in range(row + 1, size):
                identity1 = self.component_lists[list_name]["Identities"][row]
                identity2 = self.component_lists[list_name]["Identities"][col]
                index1 = self.thermopack.getcompindex(identity1)
                index2 = self.thermopack.getcompindex(identity2)

                hv_param = self.thermopack.get_hv_param(index1, index2)
                alpha_ij, alpha_ji, a_ij, a_ji, b_ij, b_ji, c_ij, c_ji = hv_param

                alpha_matrix_data[row][col] = alpha_ij
                alpha_matrix_data[col][row] = alpha_ji
                a_matrix_data[row][col] = a_ij
                a_matrix_data[col][row] = a_ji
                b_matrix_data[row][col] = b_ij
                b_matrix_data[col][row] = b_ji
                c_matrix_data[row][col] = c_ij
                c_matrix_data[col][row] = c_ji

        for row in range(size):
            alpha_matrix_data[row][row] = 0.0
            a_matrix_data[row][row] = 0.0
            b_matrix_data[row][row] = 0.0
            c_matrix_data[row][row] = 0.0

        alpha_matrix_data = alpha_matrix_data.tolist()
        a_matrix_data = a_matrix_data.tolist()
        b_matrix_data = b_matrix_data.tolist()
        c_matrix_data = c_matrix_data.tolist()
        self.component_lists[list_name]["Coefficient matrices"]["alpha"] = alpha_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["A"] = a_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["B"] = b_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["C"] = c_matrix_data

    def show_correct_tab_widget(self, list_item):
        if not list_item:
            return
        index = self.tab_stack_indices[list_item.text()]
        self.tab_stack.setCurrentIndex(index)

    def change_coeff(self, item, table_name):
        # Item has to be changed in table, self.data and be set in thermopack
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.component_lists[component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value

        alpha_matrix = self.component_lists[component_list]["Coefficient matrices"]["alpha"]
        a_matrix = self.component_lists[component_list]["Coefficient matrices"]["A"]
        b_matrix = self.component_lists[component_list]["Coefficient matrices"]["B"]
        c_matrix = self.component_lists[component_list]["Coefficient matrices"]["C"]

        alpha_ij = alpha_matrix[row][col]
        alpha_ji = alpha_matrix[col][row]
        a_ij = a_matrix[row][col]
        a_ji = a_matrix[col][row]
        b_ij = b_matrix[row][col]
        b_ji = b_matrix[col][row]
        c_ij = c_matrix[row][col]
        c_ji = c_matrix[col][row]

        identity1 = self.component_lists[component_list]["Identities"][row]
        identity2 = self.component_lists[component_list]["Identities"][col]
        index1 = self.thermopack.getcompindex(identity1)
        index2 = self.thermopack.getcompindex(identity2)
        self.thermopack.set_hv_param(index1, index2, alpha_ij, alpha_ji, a_ij, a_ji, b_ij, b_ji, c_ij, c_ji)


class HV2BinaryCoefficientsWidget(ParametersWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/hv2_bin_coeff_widget.ui", self)
        self.tab_stack_indices = {}

        self.composition_list.currentItemChanged.connect(self.show_correct_tab_widget)

    def init_widget(self, data, settings_name):
        self.settings = data["Model setups"][settings_name]
        self.thermopack = self.get_thermopack()

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for list_name in self.component_lists.keys():
            if list_name not in list_names:
                self.component_lists[list_name]["Coefficient matrices"] = {}

                # Create one tab for each list in a tab widget. The dict keeps track of the tab's index
                self.calculate_matrix_data(list_name)

                self.alpha_table = self.get_table(list_name, "alpha")
                self.a_table = self.get_table(list_name, "A")
                self.b_table = self.get_table(list_name, "B")
                self.c_table = self.get_table(list_name, "C")

                self.alpha_table.itemChanged.connect(lambda item: self.change_coeff(item, "alpha"))
                self.a_table.itemChanged.connect(lambda item: self.change_coeff(item, "A"))
                self.b_table.itemChanged.connect(lambda item: self.change_coeff(item, "B"))
                self.c_table.itemChanged.connect(lambda item: self.change_coeff(item, "C"))

                tab_widget = HV2TabWidget(self.alpha_table, self.a_table, self.b_table, self.c_table)

                # Keep track of tabs in stack
                index = self.tab_stack.addWidget(tab_widget)
                self.tab_stack_indices[list_name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name):
        # Creating 2D arrays to be stored in self.data
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        alpha_matrix_data = np.zeros(shape=(size, size))
        a_matrix_data = np.zeros(shape=(size, size))
        b_matrix_data = np.zeros(shape=(size, size))
        c_matrix_data = np.zeros(shape=(size, size))

        self.init_thermopack(list_name)

        for row in range(size - 1):
            for col in range(row + 1, size):
                identity1 = self.component_lists[list_name]["Identities"][row]
                identity2 = self.component_lists[list_name]["Identities"][col]
                index1 = self.thermopack.getcompindex(identity1)
                index2 = self.thermopack.getcompindex(identity2)
                hv_param = self.thermopack.get_hv_param(index1, index2)
                alpha_ij, alpha_ji, a_ij, a_ji, b_ij, b_ji, c_ij, c_ji = hv_param

                alpha_matrix_data[row][col] = alpha_ij
                alpha_matrix_data[col][row] = alpha_ji
                a_matrix_data[row][col] = a_ij
                a_matrix_data[col][row] = a_ji
                b_matrix_data[row][col] = b_ij
                b_matrix_data[col][row] = b_ji
                c_matrix_data[row][col] = c_ij
                c_matrix_data[col][row] = c_ji

        for row in range(size):
            alpha_matrix_data[row][row] = 0.0
            a_matrix_data[row][row] = 0.0
            b_matrix_data[row][row] = 0.0
            c_matrix_data[row][row] = 0.0

        alpha_matrix_data = alpha_matrix_data.tolist()
        a_matrix_data = a_matrix_data.tolist()
        b_matrix_data = b_matrix_data.tolist()
        c_matrix_data = c_matrix_data.tolist()
        self.component_lists[list_name]["Coefficient matrices"]["alpha"] = alpha_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["A"] = a_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["B"] = b_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["C"] = c_matrix_data

    def show_correct_tab_widget(self, list_item):
        if not list_item:
            return
        index = self.tab_stack_indices[list_item.text()]
        self.tab_stack.setCurrentIndex(index)

    def change_coeff(self, item, table_name):
        # Item has to be changed in table, self.data and be set in thermopack
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.component_lists[component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value

        alpha_matrix = self.component_lists[component_list]["Coefficient matrices"]["alpha"]
        a_matrix = self.component_lists[component_list]["Coefficient matrices"]["A"]
        b_matrix = self.component_lists[component_list]["Coefficient matrices"]["B"]
        c_matrix = self.component_lists[component_list]["Coefficient matrices"]["C"]

        alpha_ij = alpha_matrix[row][col]
        alpha_ji = alpha_matrix[col][row]
        a_ij = a_matrix[row][col]
        a_ji = a_matrix[col][row]
        b_ij = b_matrix[row][col]
        b_ji = b_matrix[col][row]
        c_ij = c_matrix[row][col]
        c_ji = c_matrix[col][row]

        identity1 = self.component_lists[component_list]["Identities"][row]
        identity2 = self.component_lists[component_list]["Identities"][col]
        index1 = self.thermopack.getcompindex(identity1)
        index2 = self.thermopack.getcompindex(identity2)
        self.thermopack.set_hv_param(index1, index2, alpha_ij, alpha_ji, a_ij, a_ji, b_ij, b_ji, c_ij, c_ji)


class HV1TabWidget(QTabWidget):
    def __init__(self, alpha_table, a_table, b_table, parent=None):
        QTabWidget.__init__(self, parent)
        self.addTab(alpha_table, u"\u03B1")
        self.addTab(a_table, "A")
        self.addTab(b_table, "B")


class HV2TabWidget(HV1TabWidget):
    def __init__(self, alpha_table, a_table, b_table, c_table, parent=None):
        HV1TabWidget.__init__(self, alpha_table, a_table, b_table, parent)
        self.addTab(c_table, "C")


class PCSAFTBinaryCoefficientsWidget(VdWBinaryCoefficientsWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)


class SAFTVRMieBinaryCoefficientsWidget(ParametersWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/saftvrmie_parameters.ui", self)
        self.tab_stack_indices = {}
        self.list_name = None
        self.component_id = None

        self.pure_param_m_label.setText("m:")
        self.pure_param_sigma_label.setText("\u03C3:")
        self.pure_param_epsilon_label.setText("\u03B5 / k:")
        self.pure_param_lambda_a_label.setText("\u03BB a:")
        self.pure_param_lambda_r_label.setText("\u03BB r:")

        self.pure_param_m_edit.editingFinished.connect(self.change_pure_param_m)
        self.pure_param_sigma_edit.editingFinished.connect(self.change_pure_param_sigma)
        self.pure_param_epsilon_edit.editingFinished.connect(self.change_pure_param_epsilon)
        self.pure_param_lambda_a_edit.editingFinished.connect(self.change_pure_param_lambda_a)
        self.pure_param_lambda_r_edit.editingFinished.connect(self.change_pure_param_lambda_r)

        self.component_btngroup = QButtonGroup()
        self.component_btngroup.buttonClicked.connect(self.show_component_pure_params)

        self.pure_params_frame.hide()

        self.composition_list.currentItemChanged.connect(self.on_chosen_composition_list)

    def init_widget(self, data, settings_name):
        self.settings = data["Model setups"][settings_name]
        self.thermopack = self.get_thermopack()
        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for list_name in self.component_lists.keys():
            if list_name not in list_names:
                self.component_lists[list_name]["Coefficient matrices"] = {}

                # Create one tab for each list in QTabWidget
                self.calculate_matrix_data(list_name)

                self.epsilon_table = self.get_table(list_name, "epsilon")
                self.sigma_table = self.get_table(list_name, "sigma")
                self.gamma_table = self.get_table(list_name, "gamma")

                self.epsilon_table.itemChanged.connect(
                    lambda item: self.change_coeff(item, self.epsilon_table, "epsilon"))
                self.sigma_table.itemChanged.connect(lambda item: self.change_coeff(item, self.sigma_table, "sigma"))
                self.gamma_table.itemChanged.connect(lambda item: self.change_coeff(item, self.gamma_table, "gamma"))

                tab_widget = SAFTVRMieTabWidget(self.epsilon_table, self.sigma_table, self.gamma_table)

                index = self.tab_stack.addWidget(tab_widget)
                self.tab_stack_indices[list_name] = index

        self.init_composition_list()

    def change_pure_param_m(self):
        value = self.pure_param_m_edit.text()
        self.pure_param_m_edit.blockSignals(True)
        self.pure_param_m_edit.clearFocus()
        self.pure_param_m_edit.blockSignals(False)

        selected_component = self.component_btngroup.checkedButton().text()
        if selected_component:
            # Validate input
            print("Setting m for", selected_component, "to", value)
            self.save_pure_fluid_params()
        else:
            return

    def change_pure_param_sigma(self):
        value = self.pure_param_sigma_edit.text()
        self.pure_param_sigma_edit.blockSignals(True)
        self.pure_param_sigma_edit.clearFocus()
        self.pure_param_sigma_edit.blockSignals(False)

        selected_component = self.component_btngroup.checkedButton().text()
        if selected_component:
            # Validate input
            print("Setting sigma for", selected_component, "to", value)
            self.save_pure_fluid_params()
        else:
            return

    def change_pure_param_epsilon(self):
        value = self.pure_param_epsilon_edit.text()
        self.pure_param_epsilon_edit.blockSignals(True)
        self.pure_param_epsilon_edit.clearFocus()
        self.pure_param_epsilon_edit.blockSignals(False)

        selected_component = self.component_btngroup.checkedButton().text()
        if selected_component:
            # Validate input
            print("Setting epsilon for", selected_component, "to", value)
            self.save_pure_fluid_params()
        else:
            return

    def change_pure_param_lambda_a(self):
        value = self.pure_param_lambda_a_edit.text()
        self.pure_param_lambda_a_edit.blockSignals(True)
        self.pure_param_lambda_a_edit.clearFocus()
        self.pure_param_lambda_a_edit.blockSignals(False)

        selected_component = self.component_btngroup.checkedButton().text()
        if selected_component:
            # Validate input
            print("Setting lambda a for", selected_component, "to", value)
            self.save_pure_fluid_params()
        else:
            return

    def change_pure_param_lambda_r(self):
        value = self.pure_param_lambda_r_edit.text()
        self.pure_param_lambda_r_edit.blockSignals(True)
        self.pure_param_lambda_r_edit.clearFocus()
        self.pure_param_lambda_r_edit.blockSignals(False)

        selected_component = self.component_btngroup.checkedButton().text()
        if selected_component:
            # Validate input
            print("Setting lambda r for", selected_component, "to", value)
            self.save_pure_fluid_params()
        else:
            return

    def save_pure_fluid_params(self):
        if not self.list_name or not self.component_id:
            return

        m = float(self.pure_param_m_edit.text())
        sigma = float(self.pure_param_sigma_edit.text())
        epsilon = float(self.pure_param_epsilon_edit.text())
        lambda_a = float(self.pure_param_lambda_a_edit.text())
        lambda_r = float(self.pure_param_lambda_r_edit.text())

        fluid_params = self.settings["Parameters"][self.list_name]["Pure fluid parameters"][self.component_id]
        fluid_params["M"] = m
        fluid_params["Sigma"] = sigma
        fluid_params["Epsilon"] = epsilon
        fluid_params["Lambda a"] = lambda_a
        fluid_params["Lambda r"] = lambda_r

    def show_component_pure_params(self, button):
        comp_name = button.text()
        list_name = self.composition_list.currentItem().text()

        self.pure_params_frame.show()

        self.init_thermopack(list_name)

        self.component_id = get_comp_id(self.component_lists[list_name], comp_name)
        comp_index = self.thermopack.getcompindex(self.component_id)

        m, sigma, eps_div_k, lambda_a, lambda_r = self.thermopack.get_pure_fluid_param(comp_index)

        self.pure_param_m_edit.setText(str(m))
        self.pure_param_sigma_edit.setText(str(sigma))
        self.pure_param_epsilon_edit.setText(str(eps_div_k))
        self.pure_param_lambda_a_edit.setText(str(lambda_a))
        self.pure_param_lambda_r_edit.setText(str(lambda_r))

    def calculate_matrix_data(self, list_name):
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        epsilon_matrix_data = np.zeros(shape=(size, size))
        sigma_matrix_data = np.zeros(shape=(size, size))
        gamma_matrix_data = np.zeros(shape=(size, size))

        self.init_thermopack(list_name)

        for row in range(size - 1):
            for col in range(row + 1, size):
                identity1 = self.component_lists[list_name]["Identities"][row]
                identity2 = self.component_lists[list_name]["Identities"][col]
                index1 = self.thermopack.getcompindex(identity1)
                index2 = self.thermopack.getcompindex(identity2)

                epsilon_kij = self.thermopack.get_eps_kij(index1, index2)
                sigma_lij = self.thermopack.get_sigma_lij(index1, index2)
                lr_gammaij = self.thermopack.get_lr_gammaij(index1, index2)

                # Sytmmetric matrix
                epsilon_matrix_data[row][col] = epsilon_kij
                epsilon_matrix_data[col][row] = epsilon_kij
                sigma_matrix_data[row][col] = sigma_lij
                sigma_matrix_data[col][row] = sigma_lij
                gamma_matrix_data[row][col] = lr_gammaij
                gamma_matrix_data[col][row] = lr_gammaij

        for row in range(size):
            epsilon_matrix_data[row][row] = 0.0
            sigma_matrix_data[row][row] = 0.0
            gamma_matrix_data[row][row] = 0.0

        epsilon_matrix_data = epsilon_matrix_data.tolist()
        sigma_matrix_data = sigma_matrix_data.tolist()
        gamma_matrix_data = gamma_matrix_data.tolist()
        self.component_lists[list_name]["Coefficient matrices"]["epsilon"] = epsilon_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["sigma"] = sigma_matrix_data
        self.component_lists[list_name]["Coefficient matrices"]["gamma"] = gamma_matrix_data

    def on_chosen_composition_list(self, list_item):
        self.list_name = list_item.text()
        if not self.list_name:
            return
        index = self.tab_stack_indices[self.list_name]
        self.tab_stack.setCurrentIndex(index)

        component_list = self.component_lists[self.list_name]["Names"]

        for button in self.component_btngroup.buttons():
            self.component_btngroup.removeButton(button)
            self.component_btn_layout.removeWidget(button)

        for comp_name in component_list:
            button = QRadioButton(comp_name)
            self.component_btngroup.addButton(button)
            self.component_btn_layout.addWidget(button)

    def change_coeff(self, item, table, table_name):
        row, col = item.row(), item.column()
        component_list = self.composition_list.currentItem().text()
        matrix = self.component_lists[component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value
        matrix[col][row] = value
        table.item(col, row).setText(str(value))

        id1 = self.component_lists[component_list]["Identities"][row]
        id2 = self.component_lists[component_list]["Identities"][col]
        index1 = self.thermopack.getcompindex(id1)
        index2 = self.thermopack.getcompindex(id2)

        if table_name == "epsilon":
            self.thermopack.set_eps_kij(index1, index2, value)

        elif table_name == "sigma":
            self.thermopack.set_sigma_lij(index1, index2, value)

        elif table_name == "gamma":
            self.thermopack.set_lr_gammaij(index1, index2, value)


class SAFTVRMieTabWidget(QTabWidget):
    def __init__(self, epsilon_table, sigma_table, gamma_table, parent=None):
        QTabWidget.__init__(self, parent)
        self.addTab(epsilon_table, "\u03B5")
        self.addTab(sigma_table, "\u03A3")
        self.addTab(gamma_table, "\u0393")
