from PyQt5.QtWidgets import QDialog, QTableWidgetItem, QHeaderView
from PyQt5.uic import loadUi
from PyQt5 import QtCore
from cubic import cubic


class VdWBinaryCoefficientsWidget(QDialog):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(parent=parent)
        loadUi("widgets/layouts/vdw_bin_coeff_widget.ui", self)

        self.settings = data["Model setups"][settings_name]
        self.component_lists = data["Component lists"]

        self.current_composition = None  # Currently chosen (str)

        self.thermopack = cubic()

        self.init_composition_list()

        self.coeff_table.itemChanged.connect(self.change_coeff)

        self.composition_list.itemSelectionChanged.connect(self.show_matrix)

    def init_thermopack(self, list_name):
        if self.settings["Model category"] == "Cubic":
            comps = ",".join(self.component_lists[list_name]["Identities"])
            eos = self.settings["EOS"]
            mixing = self.settings["Model options"]["Mixing rule"]
            alpha = self.settings["Model options"]["Alpha correlation"]
            ref = self.settings["Model options"]["Reference"]

            self.thermopack.init(comps, eos, mixing=mixing, alpha=alpha, parameter_reference=ref)

    def init_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            # All coefficient matrices are created and saved
            self.composition_list.addItem(list_name)
            self.init_create_matrix_data(list_name)

    def init_create_matrix_data(self, component_list_name):
        # Creating 2D array
        matrix_data = []
        component_list = self.component_lists[component_list_name]["Names"]
        size = len(component_list)

        self.init_thermopack(component_list_name)

        for row in range(size):
            coeffs = []
            for col in range(size):
                identity1 = self.component_lists[component_list_name]["Identities"][row]
                identity2 = self.component_lists[component_list_name]["Identities"][col]
                index1 = self.thermopack.getcompindex(identity1)
                index2 = self.thermopack.getcompindex(identity2)
                coeff = self.thermopack.get_kij(index1, index2)
                coeffs.append(coeff)
            matrix_data.append(coeffs)

        self.component_lists[component_list_name]["Coefficient matrix"] = matrix_data

    def show_matrix(self):
        item = self.composition_list.currentItem()
        if not item:
            return

        composition = self.component_lists[item.text()]["Names"]
        matrix_data = self.component_lists[item.text()]["Coefficient matrix"]
        size = len(composition)

        self.coeff_table.setRowCount(size)
        self.coeff_table.setColumnCount(size)

        # Insert coefficients into table
        for row in range(size):
            self.coeff_table.setVerticalHeaderItem(row, QTableWidgetItem(composition[row]))

        for col in range(size):
            self.coeff_table.setHorizontalHeaderItem(col, QTableWidgetItem(composition[col]))

        for i in range(size):
            for j in range(size):

                if i == j:
                    coeff = "-"
                else:
                    coeff = matrix_data[i][j]

                item = QTableWidgetItem(str(coeff))
                item.setTextAlignment(QtCore.Qt.AlignCenter)

                self.coeff_table.blockSignals(True)
                self.coeff_table.setItem(i, j, item)
                self.coeff_table.blockSignals(False)

                # Matrix is symmetric, so these items can't be edited
                if i >= j:
                    item.setFlags(QtCore.Qt.NoItemFlags)

        header = self.coeff_table.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeToContents)

    def update_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            # All coefficient matrices are created and saved
            self.composition_list.addItem(list_name)
            self.init_create_matrix_data(list_name)

    def change_coeff(self, item):
        row = item.row()
        col = item.column()

        component_list = self.composition_list.currentItem().text()
        matrix = self.component_lists[component_list]["Coefficient matrix"]

        try:
            value = float(item.text().replace(",", "."))
        except ValueError:
            previous_value = matrix[row][col]
            item.setText(str(previous_value))
            return

        matrix[row][col] = value

        # Symmetric matrix
        matrix[col][row] = value
        self.coeff_table.item(col, row).setText(str(value))


class HV1BinaryCoefficientsWidget(QDialog):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(parent=parent)
        loadUi("widgets/layouts/hv1_bin_coeff_widget.ui", self)

        self.settings = data["Model setups"][settings_name]
        self.component_lists = data["Component lists"]

    def init_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)

    def update_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)


class HV2BinaryCoefficientsWidget(QDialog):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(parent=parent)
        loadUi("widgets/layouts/hv2_bin_coeff_widget.ui", self)

        self.settings = data["Model setups"][settings_name]
        self.component_lists = data["Component lists"]

    def init_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)

    def update_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)
