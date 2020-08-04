from PyQt5.QtWidgets import QWidget, QTableWidget, QTableWidgetItem, QHeaderView, QTabWidget, QRadioButton, QButtonGroup
from PyQt5.uic import loadUi
from PyQt5 import QtCore

from gui.utils import get_thermopack, init_thermopack, get_comp_id, FloatValidator

import numpy as np


# TODO: Når fanen byttes skal lista deselectes og table_stack --> page0 (tom)
# TODO: Lage en bedre klassestruktur med arv.


class ParametersWidget(QWidget):
    """
    Base class for the widgets for seeing and editing the interaction (and other) parameters.
    These widgets contain a list of available compositions, and a vies for the different parameters
    """

    def __init__(self, data, settings_name, parent=None):
        super().__init__(parent=parent)

        self.settings = data["Model setups"][settings_name]
        self.component_lists = data["Component lists"]

    def init_composition_list(self):
        """
        Displays available compositions in a QListWidget
        """
        self.composition_list.blockSignals(True)
        self.composition_list.clear()
        self.composition_list.blockSignals(False)
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)

    def get_table(self, list_name, table_name):
        """
        Returns a QTableWidget showing an interaction parameter matrix for a given composition
        :param list_name: str, name of component list of the corresponding table
        :param table_name: str, name of table (could be K, alpha, A, B, C. epsilon, sigma, gamma dep on mixing rule)
        :return: QTableWidget, table containing the correct parameters
        """
        composition = self.component_lists[list_name]["Names"]
        matrix_data = self.settings["Parameters"][list_name]["Coefficient matrices"][table_name]
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

                if table_name in ["VDW K", "PC-SAFT K", "SAFT-VR Mie Epsilon", "SAFT-VR Mie Sigma",
                                  "SAFT-VR Mie Gamma"]:
                    # Matrix is symmetric, so these items can't be edited
                    if i >= j:
                        item.setFlags(QtCore.Qt.NoItemFlags)

        header = table.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeToContents)

        return table


class VdWParametersWidget(ParametersWidget):
    """
    Widget for changing interaction coefficients for the VdW mixing rule
    """

    # Should be reinitiated every time opened, so that coefficients are correct if model is changed
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/vdw_bin_coeff_widget.ui", self)

        self.composition_list.currentItemChanged.connect(self.show_correct_matrix)
        self.stack_indices = {}

    def init_widget(self, data, settings_name):
        """
        Populates the list with the available compositions, calculates interaction parameters, and creates a table
        to display the matrix
        :param data: Session data
        :param settings_name: Name of the current model setup
        :return:
        """
        self.settings = data["Model setups"][settings_name]
        self.thermopack = get_thermopack(category=self.settings["Model category"])

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for name in self.component_lists.keys():

            if name not in self.settings["Parameters"].keys():
                self.settings["Parameters"][name] = {"Coefficient matrices": {}}

            existing_matrices = self.settings["Parameters"][name]["Coefficient matrices"].keys()

            if "VDW K" not in existing_matrices:
                # Create one table for each list in a stacked widget if it does not already exist
                # The dict keeps track of the table's index in the stack

                # Create table
                self.calculate_matrix_data(name, settings_name)
                table = self.get_table(name, "VDW K")
                table.itemChanged.connect(lambda item: self.change_coeff(item, "VDW K"))

                # Keep track of table in stack
                index = self.table_stack.addWidget(table)
                self.stack_indices[name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name, reset=False):
        """
        Calculates binary coefficients for all composition lists and stores them
        :param list_name: str, Name of component list
        :param reset: bool, If True, existing matrix data will be overloaded by thermopack's default values
        """
        init_thermopack(self.thermopack, self.component_lists[list_name], list_name, self.settings)

        coeff_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"]
        if "VDW K" in coeff_matrices.keys() and not reset:
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
        coeff_matrices["VDW K"] = matrix_data

    def show_correct_matrix(self, list_item):
        """
        Shows the correct interaction coefficient matrix when a composition list is chosen
        :param list_item: Name of composition list
        """
        if not list_item:
            return
        list_name = list_item.text()
        index = self.stack_indices[list_name]
        self.table_stack.setCurrentIndex(index)

    def change_coeff(self, item, table_name):
        """
        Changes a value (interaction coefficient) in the given table
        :param item: New value
        :param table_name: Name of coefficient matrix (VDW K, PC-SAFT Sigma, ...)
        """
        # Item has to be changed in table and self.data
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.settings["Parameters"][component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value

        matrix[col][row] = value
        item.tableWidget().blockSignals(True)
        item.tableWidget().item(col, row).setText(str(value))
        item.tableWidget().blockSignals(False)


class HV1ParametersWidget(ParametersWidget):
    """
    Widget for changing interaction coefficients for the HV1 mixing rule
    """

    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/hv1_bin_coeff_widget.ui", self)
        self.tab_stack_indices = {}

        self.composition_list.currentItemChanged.connect(self.show_correct_tab_widget)

    def init_widget(self, data, settings_name):
        """
        Populates the list with the available compositions, calculates interaction parameters, and creates a table
        to display the matrix
        :param data: Session data
        :param settings_name: Name of the current model setup
        """
        self.settings = data["Model setups"][settings_name]
        self.thermopack = get_thermopack(category=self.settings["Model category"])

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for list_name in self.component_lists.keys():

            if list_name not in self.settings["Parameters"].keys():
                self.settings["Parameters"][list_name] = {"Coefficient matrices": {}}

            existing_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"].keys()

            if "HV1 Alpha" not in existing_matrices:
                # Create one tab for each list in a tab widget. The dict keeps track of the tab's index
                self.calculate_matrix_data(list_name)

                self.alpha_table = self.get_table(list_name, "HV1 Alpha")
                self.a_table = self.get_table(list_name, "HV1 A")
                self.b_table = self.get_table(list_name, "HV1 B")
                self.c_table = self.get_table(list_name, "HV1 C")

                self.alpha_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV1 Alpha"))
                self.a_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV1 A"))
                self.b_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV1 B"))
                self.c_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV1 C"))

                tab_widget = HV1TabWidget(self.alpha_table, self.a_table, self.b_table)

                # Keep track of tabs in stack
                index = self.tab_stack.addWidget(tab_widget)
                self.tab_stack_indices[list_name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name):
        """
        Calculates binary coefficients for all composition lists and stores them in the session data.
        :param list_name: str, Name of component list
        """
        # Creating 2D arrays to be stored in self.data
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        alpha_matrix_data = np.zeros(shape=(size, size))
        a_matrix_data = np.zeros(shape=(size, size))
        b_matrix_data = np.zeros(shape=(size, size))
        c_matrix_data = np.zeros(shape=(size, size))

        init_thermopack(self.thermopack, self.component_lists[list_name], list_name, self.settings)

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

        coeff_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"]

        coeff_matrices["HV1 Alpha"] = alpha_matrix_data
        coeff_matrices["HV1 A"] = a_matrix_data
        coeff_matrices["HV1 B"] = b_matrix_data
        coeff_matrices["HV1 C"] = c_matrix_data

    def show_correct_tab_widget(self, list_item):
        """
        There is one QTabWidget for each composition. When a composition is chosen, the correct tab is shown
        :param list_item: Name of composition
        """
        if not list_item:
            return
        index = self.tab_stack_indices[list_item.text()]
        self.tab_stack.setCurrentIndex(index)

    def change_coeff(self, item, table_name):
        """
        Changes a value (interaction coefficient) in the given table. Due to the way the Python interface for
        thermopack works, all values have to be set simultaneously
        :param item: New value
        :param table_name: Name of coefficient matrix (K, A, B, C, Sigma, ...)
        """
        # Item has to be changed in table, self.data and be set in thermopack
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.settings["Parameters"][component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value


class HV2ParametersWidget(ParametersWidget):
    """
    Widget for changing interaction coefficients for the HV2 mixing rule
    """

    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/hv2_bin_coeff_widget.ui", self)
        self.tab_stack_indices = {}

        self.composition_list.currentItemChanged.connect(self.show_correct_tab_widget)

    def init_widget(self, data, settings_name):
        """
        Populates the list with the available compositions, calculates interaction parameters, and creates a table
        to display the matrix
        :param data: Session data
        :param settings_name: Name of the current model setup
        :return:
        """
        self.settings = data["Model setups"][settings_name]
        self.thermopack = get_thermopack(category=self.settings["Model category"])

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for list_name in self.component_lists.keys():

            if list_name not in self.settings["Parameters"].keys():
                self.settings["Parameters"][list_name] = {"Coefficient matrices": {}}

            existing_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"].keys()

            if "HV2 Alpha" not in existing_matrices:
                # Create one tab for each list in a tab widget. The dict keeps track of the tab's index
                self.calculate_matrix_data(list_name)

                self.alpha_table = self.get_table(list_name, "HV2 Alpha")
                self.a_table = self.get_table(list_name, "HV2 A")
                self.b_table = self.get_table(list_name, "HV2 B")
                self.c_table = self.get_table(list_name, "HV2 C")

                self.alpha_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV2 Alpha"))
                self.a_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV2 A"))
                self.b_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV2 B"))
                self.c_table.itemChanged.connect(lambda item: self.change_coeff(item, "HV2 C"))

                tab_widget = HV2TabWidget(self.alpha_table, self.a_table, self.b_table, self.c_table)

                # Keep track of tabs in stack
                index = self.tab_stack.addWidget(tab_widget)
                self.tab_stack_indices[list_name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name):
        """
        Calculates binary coefficients for all composition lists and stores them in the session data.
        :param list_name: str, Name of component list
        """
        # Creating 2D arrays to be stored in self.data
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        alpha_matrix_data = np.zeros(shape=(size, size))
        a_matrix_data = np.zeros(shape=(size, size))
        b_matrix_data = np.zeros(shape=(size, size))
        c_matrix_data = np.zeros(shape=(size, size))

        init_thermopack(self.thermopack, self.component_lists[list_name], list_name, self.settings)

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

        coeff_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"]

        coeff_matrices["HV2 Alpha"] = alpha_matrix_data
        coeff_matrices["HV2 A"] = a_matrix_data
        coeff_matrices["HV2 B"] = b_matrix_data
        coeff_matrices["HV2 C"] = c_matrix_data

    def show_correct_tab_widget(self, list_item):
        """
        There is one QTabWidget for each composition. When a composition is chosen, the correct tab is shown
        :param list_item: Name of composition
        """
        if not list_item:
            return
        index = self.tab_stack_indices[list_item.text()]
        self.tab_stack.setCurrentIndex(index)

    def change_coeff(self, item, table_name):
        """
        Changes a value (interaction coefficient) in the given table. Due to the way the Python interface for
        thermopack works, all values have to be set simultaneously
        :param item: New value
        :param table_name: Name of coefficient matrix (K, A, B, C, Sigma, ...)
        """
        # Item has to be changed in table, self.data and be set in thermopack
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.settings["Parameters"][component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value


class PCSAFTParametersWidget(VdWParametersWidget):
    """
    Widget for changing interaction coefficients for the PC-SAFT model
    """

    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)

    def init_widget(self, data, settings_name):
        """
        Populates the list with the available compositions, calculates interaction parameters, and creates a table
        to display the matrix
        :param data: Session data
        :param settings_name: Name of the current model setup
        :return:
        """
        self.settings = data["Model setups"][settings_name]
        self.thermopack = get_thermopack(category=self.settings["Model category"])

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for name in self.component_lists.keys():

            if name not in self.settings["Parameters"].keys():
                self.settings["Parameters"][name] = {"Coefficient matrices": {}}

            existing_matrices = self.settings["Parameters"][name]["Coefficient matrices"].keys()

            if "PC-SAFT K" not in existing_matrices:
                # Create one table for each list in a stacked widget.
                # The dict keeps track of the table's index in the stack

                # Create table
                self.calculate_matrix_data(name, settings_name)
                table = self.get_table(name, "PC-SAFT K")
                table.itemChanged.connect(lambda item: self.change_coeff(item, "PC-SAFT K"))

                # Keep track of table in stack
                index = self.table_stack.addWidget(table)
                self.stack_indices[name] = index

        self.init_composition_list()

    def calculate_matrix_data(self, list_name, reset=False):
        """
        Calculates binary coefficients for all composition lists and stores them
        :param list_name: str, Name of component list
        :param reset: bool, If True, existing matrix data will be overloaded by thermopack's default values
        """
        init_thermopack(self.thermopack, self.component_lists[list_name], list_name, self.settings)

        coeff_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"]
        if "PC-SAFT K" in coeff_matrices.keys() and not reset:
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
        coeff_matrices["PC-SAFT K"] = matrix_data


class SAFTVRMieParametersWidget(ParametersWidget):
    """
    Widget for changing interaction coefficients and pure fluid parameters for the SAFT-VR Mie model
    """

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

        # Validators for input
        self.float_validator = FloatValidator()
        self.set_validators()

        self.pure_param_m_edit.editingFinished.connect(self.save_pure_fluid_params)
        self.pure_param_sigma_edit.editingFinished.connect(self.save_pure_fluid_params)
        self.pure_param_epsilon_edit.editingFinished.connect(self.save_pure_fluid_params)
        self.pure_param_lambda_a_edit.editingFinished.connect(self.save_pure_fluid_params)
        self.pure_param_lambda_r_edit.editingFinished.connect(self.save_pure_fluid_params)

        self.component_btngroup = QButtonGroup()
        self.component_btngroup.buttonClicked.connect(self.show_component_pure_params)

        self.pure_params_frame.hide()

        self.composition_list.currentItemChanged.connect(self.on_chosen_composition_list)

    def init_widget(self, data, settings_name):
        """
        Populates the list with the available compositions, calculates interaction parameters, and creates a table
        to display the matrix
        :param data: Session data
        :param settings_name: Name of the current model setup
        :return:
        """
        self.settings = data["Model setups"][settings_name]
        self.thermopack = get_thermopack(category=self.settings["Model category"])

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i).text())

        for list_name in self.component_lists.keys():

            if list_name in self.settings["Parameters"].keys():
                if "Pure fluid parameters" not in self.settings["Parameters"][list_name].keys():
                    self.settings["Parameters"][list_name]["Pure fluid parameters"] = {}

            else:
                self.settings["Parameters"][list_name] = {
                    "Coefficient matrices": {},
                    "Pure fluid parameters": {}
                }

            existing_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"].keys()

            if "SAFT-VR Mie Epsilon" not in existing_matrices:
                # Create one tab for each list in QTabWidget
                self.calculate_matrix_data(list_name)

                self.epsilon_table = self.get_table(list_name, "SAFT-VR Mie Epsilon")
                self.sigma_table = self.get_table(list_name, "SAFT-VR Mie Sigma")
                self.gamma_table = self.get_table(list_name, "SAFT-VR Mie Gamma")

                self.epsilon_table.itemChanged.connect(
                    lambda item: self.change_coeff(item, "SAFT-VR Mie Epsilon"))
                self.sigma_table.itemChanged.connect(
                    lambda item: self.change_coeff(item, "SAFT-VR Mie Sigma"))
                self.gamma_table.itemChanged.connect(
                    lambda item: self.change_coeff(item, "SAFT-VR Mie Gamma"))

                tab_widget = SAFTVRMieTabWidget(self.epsilon_table, self.sigma_table, self.gamma_table)

                index = self.tab_stack.addWidget(tab_widget)
                self.tab_stack_indices[list_name] = index

        self.init_composition_list()

    def set_validators(self):
        """
        Sets a validator for input fields to ensure a valid float input
        """
        self.pure_param_m_edit.setValidator(self.float_validator)
        self.pure_param_sigma_edit.setValidator(self.float_validator)
        self.pure_param_epsilon_edit.setValidator(self.float_validator)
        self.pure_param_lambda_a_edit.setValidator(self.float_validator)
        self.pure_param_lambda_r_edit.setValidator(self.float_validator)

    def save_pure_fluid_params(self):
        """
        Saves the current pure fluid parameters to the session data
        """
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
        """
        When a component is chosen, the corresponding pure fluid parameters are displayed
        :param button: Clicked radio button giving the chosen component
        """
        comp_name = button.text()
        list_name = self.composition_list.currentItem().text()

        self.pure_params_frame.show()

        init_thermopack(self.thermopack, self.component_lists[list_name], list_name, self.settings)

        self.component_id = get_comp_id(self.component_lists[list_name], comp_name)
        comp_index = self.thermopack.getcompindex(self.component_id)

        m, sigma, eps_div_k, lambda_a, lambda_r = self.thermopack.get_pure_fluid_param(comp_index)

        self.pure_param_m_edit.setText(str(m))
        self.pure_param_sigma_edit.setText(str(sigma))
        self.pure_param_epsilon_edit.setText(str(eps_div_k))
        self.pure_param_lambda_a_edit.setText(str(lambda_a))
        self.pure_param_lambda_r_edit.setText(str(lambda_r))

    def calculate_matrix_data(self, list_name):
        """
        Calculates binary coefficients for all composition lists and stores them in the session data.
        :param list_name: str, Name of component list
        """
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        epsilon_matrix_data = np.zeros(shape=(size, size))
        sigma_matrix_data = np.zeros(shape=(size, size))
        gamma_matrix_data = np.zeros(shape=(size, size))

        init_thermopack(self.thermopack, self.component_lists[list_name], list_name, self.settings)

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

        coeff_matrices = self.settings["Parameters"][list_name]["Coefficient matrices"]

        coeff_matrices["SAFT-VR Mie Epsilon"] = epsilon_matrix_data
        coeff_matrices["SAFT-VR Mie Sigma"] = sigma_matrix_data
        coeff_matrices["SAFT-VR Mie Gamma"] = gamma_matrix_data

    def on_chosen_composition_list(self, list_item):
        """
        Show the correct tab with interaction coefficient matrices, and display radio buttons with all components
        in the given composition.
        :param list_item: Chosen composition
        """
        self.list_name = list_item.text()
        if not self.list_name:
            return
        index = self.tab_stack_indices[self.list_name]
        self.tab_stack.setCurrentIndex(index)

        self.pure_params_frame.hide()

        component_list = self.component_lists[self.list_name]["Names"]

        for button in self.component_btngroup.buttons():
            self.component_btngroup.removeButton(button)
            self.component_btn_layout.removeWidget(button)
            button.hide()

        if "Pure fluid parameters" not in self.settings["Parameters"][self.list_name].keys():
            self.settings["Parameters"][self.list_name]["Pure fluid parameters"] = {}

        pure_fluid_params = self.settings["Parameters"][self.list_name]["Pure fluid parameters"]

        for comp_name in component_list:
            comp_id = get_comp_id(self.component_lists[self.list_name], comp_name)

            if comp_id not in pure_fluid_params.keys():
                pure_fluid_params[comp_id] = {
                    "M": None,
                    "Sigma": None,
                    "Epsilon": None,
                    "Lambda a": None,
                    "Lambda r": None
                }

            button = QRadioButton(comp_name)
            self.component_btngroup.addButton(button)
            self.component_btn_layout.addWidget(button)

    def change_coeff(self, item, table_name):
        """
        Changes a value (interaction coefficient) in the given table. Due to the way the Python interface for
        thermopack works, all values have to be set simultaneously
        :param item: New value
        :param table_name: Name of coefficient matrix (VDW K, HV1 A, SAFT-VR Mie Sigma, ...)
        """
        row, col = item.row(), item.column()
        component_list = self.composition_list.currentItem().text()
        matrix = self.settings["Parameters"][component_list]["Coefficient matrices"][table_name]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value
        matrix[col][row] = value
        item.tableWidget().item(col, row).setText(str(value))


class HV1TabWidget(QTabWidget):
    """
    Contains tabs for the four different interaction coefficient matrices for the HV1 mixing rule
    """

    def __init__(self, alpha_table, a_table, b_table, parent=None):
        QTabWidget.__init__(self, parent)
        self.addTab(alpha_table, u"\u03B1")
        self.addTab(a_table, "A")
        self.addTab(b_table, "B")


class HV2TabWidget(HV1TabWidget):
    """
    Contains tabs for the four different interaction coefficient matrices for the HV2 mixing rule
    """

    def __init__(self, alpha_table, a_table, b_table, c_table, parent=None):
        HV1TabWidget.__init__(self, alpha_table, a_table, b_table, parent)
        self.addTab(c_table, "C")


class SAFTVRMieTabWidget(QTabWidget):
    """
    Contains tabs for the three different interaction coefficient matrices for the SAFT-VR Mie model
    """

    def __init__(self, epsilon_table, sigma_table, gamma_table, parent=None):
        QTabWidget.__init__(self, parent)
        self.addTab(epsilon_table, "\u03B5")
        self.addTab(sigma_table, "\u03A3")
        self.addTab(gamma_table, "\u0393")
