from PyQt5.QtWidgets import QWidget, QTableWidget, QTableWidgetItem, QHeaderView, QTabWidget
from PyQt5.uic import loadUi
from PyQt5 import QtCore
from cubic import cubic
from cpa import cpa
import numpy as np

# TODO: Ikke sikkert at disse trenger en self.thermopack. Koeffisientene kan fint regnes ut uten å instansiere den.
#  Foreløpig må den være der ettersom jeg ikke har en annen måte å få tak i dem på

# TODO: Run get_matrix_data and set_matrix data even when the tab is not opened. (e.g. when Plot mode is initiated)

# TODO: Når fanen byttes skal lista deselectes og table_stack --> page0 (tom)

# TODO: Methane + CO2 + CPA --> error når jeg switcher til bin coeff tab

# TODO: Få HV1 og HV2 til å arve samme hovedklasse. Nå er alt likt utenom et tall ellerno...


class BinaryCoefficientsWidget(QWidget):
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
        else:
            return None

    def init_thermopack(self, list_name):
        # This must be run before coefficients are calculated
        if isinstance(self.thermopack, cubic) or isinstance(self.thermopack, cpa):
            comps = ",".join(self.component_lists[list_name]["Identities"])
            eos = self.settings["EOS"]
            mixing = self.settings["Model options"]["Mixing rule"]
            alpha = self.settings["Model options"]["Alpha correlation"]
            ref = self.settings["Model options"]["Reference"]

            if mixing == "HV1":
                mixing = "HV"

            self.thermopack.init(comps, eos, mixing=mixing, alpha=alpha, parameter_reference=ref)

    def init_composition_list(self):
        self.composition_list.clear()
        for list_name in self.component_lists.keys():
            self.composition_list.addItem(list_name)

    def get_table(self, list_name, table_name):
        """
        :param list_name: str, name of component list to which the table is corresponsing
        :param table_name: str, name of table (could be K, alpha, A, B, C depending on mixing rule)
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
                    coeff = "-"
                else:
                    coeff = matrix_data[i][j]

                item = QTableWidgetItem(str(coeff))
                item.setTextAlignment(QtCore.Qt.AlignCenter)

                table.blockSignals(True)
                table.setItem(i, j, item)
                table.blockSignals(False)

                if table_name == "K":
                    # Matrix is symmetric, so these items can't be edited
                    if i >= j:
                        item.setFlags(QtCore.Qt.NoItemFlags)

        header = table.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeToContents)

        return table


class VdWBinaryCoefficientsWidget(BinaryCoefficientsWidget):
    # Should be reinitiated every time opened, so that coefficients are correct if model is changed
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/vdw_bin_coeff_widget.ui", self)

        self.composition_list.currentItemChanged.connect(self.show_correct_matrix)

    def init_widget(self):
        self.thermopack = self.get_thermopack()

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i))

        for name in self.component_lists.keys():
            if name not in list_names:
                self.component_lists[name]["Coefficient matrices"] = {}

        self.init_composition_list()
        self.stack_indices = {}

        # Create one table for each list in a stacked widget. The dict keeps track of the table's index in the stack
        for list_name in self.component_lists.keys():
            # Create table
            self.calculate_matrix_data(list_name)
            table = self.get_table(list_name, "K")
            table.itemChanged.connect(lambda item: self.change_coeff(item, "K"))

            # Keep track of table in stack
            index = self.table_stack.addWidget(table)
            self.stack_indices[list_name] = index

    def calculate_matrix_data(self, list_name):
        """
        :param list_name: str
        Calculates binary coefficients for all composition lists and stores them
        """
        # Creating 2D array to be stored in self.data
        component_list = self.component_lists[list_name]["Names"]
        size = len(component_list)
        matrix_data = np.zeros(shape=(size, size))

        self.init_thermopack(list_name)

        for row in range(size - 1):
            for col in range(row + 1, size):
                identity1 = self.component_lists[list_name]["Identities"][row]
                identity2 = self.component_lists[list_name]["Identities"][col]
                index1 = self.thermopack.getcompindex(identity1)
                index2 = self.thermopack.getcompindex(identity2)
                coeff = self.thermopack.get_kij(index1, index2)
                matrix_data[row][col] = coeff
                matrix_data[col][row] = coeff

        for row in range(size):
            matrix_data[row][row] = 0

        matrix_data = matrix_data.tolist()
        self.component_lists[list_name]["Coefficient matrices"]["K"] = matrix_data

    def show_correct_matrix(self, list_item):
        if not list_item:
            return
        list_name = list_item.text()
        index = self.stack_indices[list_name]
        self.table_stack.setCurrentIndex(index)

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

        identity1 = self.component_lists[component_list]["Identities"][row]
        identity2 = self.component_lists[component_list]["Identities"][col]
        index1 = self.thermopack.getcompindex(identity1)
        index2 = self.thermopack.getcompindex(identity2)
        self.thermopack.set_kij(index1, index2, value)

        # Symmetric matrix
        matrix[col][row] = value
        self.coeff_table.item(col, row).setText(str(value))


class HV1BinaryCoefficientsWidget(BinaryCoefficientsWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/hv1_bin_coeff_widget.ui", self)

        self.composition_list.currentItemChanged.connect(self.show_correct_tab_widget)

    def init_widget(self):
        self.thermopack = self.get_thermopack()

        self.init_composition_list()
        self.tab_stack_indices = {}

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i))

        for name in self.component_lists.keys():
            if name not in list_names:
                self.component_lists[name]["Coefficient matrices"] = {}

        # Create one tab for each list in a tab widget. The dict keeps track of the tab's index
        for list_name in self.component_lists.keys():
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
            alpha_matrix_data[row][row] = 0
            a_matrix_data[row][row] = 0
            b_matrix_data[row][row] = 0
            c_matrix_data[row][row] = 0

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

        if table_name == "alpha":
            self.alpha_table.item(col, row).setText(str(value))
        elif table_name == "A":
            self.a_table.item(col, row).setText(str(value))
        elif table_name == "B":
            self.b_table.item(col, row).setText(str(value))
        elif table_name == "C":
            self.c_table.item(col, row).setText(str(value))


class HV2BinaryCoefficientsWidget(BinaryCoefficientsWidget):
    def __init__(self, data, settings_name, parent=None):
        super().__init__(data, settings_name, parent)
        loadUi("widgets/layouts/hv2_bin_coeff_widget.ui", self)

        self.composition_list.currentItemChanged.connect(self.show_correct_tab_widget)

    def init_widget(self):
        self.thermopack = self.get_thermopack()

        self.init_composition_list()
        self.tab_stack_indices = {}

        list_names = []
        for i in range(self.composition_list.count()):
            list_names.append(self.composition_list.item(i))

        for name in self.component_lists.keys():
            if name not in list_names:
                self.component_lists[name]["Coefficient matrices"] = {}

        # Create one tab for each list in a tab widget. The dict keeps track of the tab's index
        for list_name in self.component_lists.keys():
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
            alpha_matrix_data[row][row] = 0
            a_matrix_data[row][row] = 0
            b_matrix_data[row][row] = 0
            c_matrix_data[row][row] = 0

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

        if table_name == "alpha":
            self.alpha_table.item(col, row).setText(str(value))
        elif table_name == "A":
            self.a_table.item(col, row).setText(str(value))
        elif table_name == "B":
            self.b_table.item(col, row).setText(str(value))
        elif table_name == "C":
            self.c_table.item(col, row).setText(str(value))


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
