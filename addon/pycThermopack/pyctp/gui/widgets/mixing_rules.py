from PyQt5.QtWidgets import QDialog, QTableWidgetItem, QHeaderView
from PyQt5.uic import loadUi
from PyQt5 import QtCore
from thermo import thermopack

# TODO: Skal være mulig å endre koeffisienter
# TODO: Mulig at alle koeffisienter nå resettes når fanen åpnes igjen.
#  Må ha en update() i stedet for init() som lager de rette matrisene


class VdWBinaryCoefficientsWidget(QDialog):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(parent=parent)
        loadUi("widgets/layouts/vdw_bin_coeff_widget.ui", self)

        self.settings = data["Model setups"][settings_name]
        self.component_lists = data["Component lists"]

        self.current_composition = None     # Currently chosen (str)

        self.thermo = thermopack()

        self.init_composition_list()

        self.composition_list.itemSelectionChanged.connect(self.show_matrix)

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

        for row in range(size):
            coeffs = []
            for col in range(size):
                coeff = self.thermo.get_kij(component_list[row], component_list[col])
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
                self.coeff_table.setItem(i, j, item)

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
