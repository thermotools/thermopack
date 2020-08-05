from PyQt5.QtWidgets import QMainWindow, QTableWidgetItem, QFileDialog, QTextEdit, QSizePolicy, QLineEdit, QLabel, \
    QActionGroup
from PyQt5.QtGui import QIcon
from PyQt5.uic import loadUi

import csv
import os

import numpy as np
import pint

from gui.widgets.units_dialog import UnitsDialog
from gui.utils import get_thermopack, init_thermopack, FloatValidator


# TODO: ENHETER: Alltid lagre verdier som SI i JSON-fila. Bruke pint-biblioteket til å konvertere til ønskede enheter
#  Kan egentlig flytte (/også ha) UnitDialog i CalcMode.
#  Alle inputs som har noe med enheter å gjøre, må endre labels 'Label' => 'Label [Unit]'
#  value --> Quantity-objekt med unit_from --> Quantity-objekt med unit_to, set i tabell med str(quantity.magnitude)

# TODO: Handle andre phase flags


class CalcMode(QMainWindow):
    """
    Calculation mode: Own window to calculate, display and save flash data
    """

    def __init__(self, data, json_file, component_list_name, model_settings_name, parent=None):
        super().__init__(parent=parent)

        loadUi("widgets/layouts/calc_mode.ui", self)
        self.setWindowTitle("Thermopack - Calculation Mode")
        self.showMaximized()

        self.set_toolbar()

        self.data = data
        self.json_file = json_file

        self.component_data = self.data["Component lists"][component_list_name]
        self.comp_list_name = component_list_name
        self.settings = self.data["Model setups"][model_settings_name]
        self.units = self.data["Units"]

        self.tp = get_thermopack(category=self.settings["Model category"])

        self.input_stack_indices = {
            "TP": 0,
            "PS": 1,
            "PH": 2,
            "UV": 3
        }

        self.units = {
            "Temperature": "K",
            "Pressure": "Pa",
            "Specific volume": "m ** 3 / mol",
            "Internal energy": "J / mol",
            "Enthalpy": "J / mol",
            "Entropy": "J / (K * mol)",
            "Gibbs energy": "J / mol",
            "Isobar heat capacity": "J / (K * mol)",
            "Isochor heat capacity": "J / (K * mol)",
            "Speed of sound": "m / s",
            "Molecular weight": "kg / mol",
            "Phase fraction": "mol / mol"
        }

        # Units registry from pint library
        self.ureg = pint.UnitRegistry()

        self.table_indices = self.get_table_indices()

        # Validator for float inputs
        self.float_validator = FloatValidator()

        self.show_input_params()
        self.init_fractions()

        self.set_validators()

        self.flash_mode_selection.currentTextChanged.connect(self.show_input_params)
        self.calculate_btn.clicked.connect(self.calculate)
        self.download_csv_btn.clicked.connect(self.export_csv)

        self.ps_initial_guess.stateChanged.connect(self.toggle_initial_guess)
        self.ph_initial_guess.stateChanged.connect(self.toggle_initial_guess)
        self.uv_t_initial_guess.stateChanged.connect(self.toggle_initial_guess)
        self.uv_p_initial_guess.stateChanged.connect(self.toggle_initial_guess)

    def set_toolbar(self):
        """
        Creates the top toolbar
        """
        # Logo
        logo = QLabel("Thermopack  |  Calculation Mode  ")
        logo.setStyleSheet("color: #FF8B06; font: 75 28pt 'Agency FB'; padding: 5px 10px 5px 10px;")

        # Top toolbar
        toolbar = self.addToolBar("Tool bar")
        toolbar.setMovable(False)
        toolbar.actionTriggered.connect(self.handle_toolbar_action)
        toolbar.setStyleSheet("padding: 5px 10px 5px 10px;")
        toolbar.addWidget(logo)
        toolbar.addSeparator()

        action_group = QActionGroup(self)
        action_group.addAction(toolbar.addAction(QIcon("icons/settings.png"), "Units"))

        self.action_close.triggered.connect(self.close)
        self.action_units.triggered.connect(self.open_units_window)
        self.action_display_settings.triggered.connect(self.display_settings)


    def handle_toolbar_action(self, action):
        """
        Calls the correct function depending on which tool icon was clicked
        :param action: Type of tool clicked
        """
        action = action.text()
        if action == "Units":
            self.open_units_window()

    def open_units_window(self):
        """
        Opens a dialog where the user can change default units for the application
        """
        units_data = self.data["Units"]
        self.dialog = UnitsDialog(units_data)
        self.dialog.show()

    def show_input_params(self, flash_mode="TP"):
        """
        Shows the correct input fields, depending on type of flash chosen
        :param flash_mode: Type of flash (TP, PS, PH, UV)
        """
        index = self.input_stack_indices[flash_mode]
        self.flash_input_stack.setCurrentIndex(index)

    def set_validators(self):
        """
        Sets a float validator to all input fields
        """
        self.tp_t_input.setValidator(self.float_validator)
        self.tp_p_input.setValidator(self.float_validator)

        self.ps_p_input.setValidator(self.float_validator)
        self.ps_s_input.setValidator(self.float_validator)
        self.ps_t_guess.setValidator(self.float_validator)

        self.ph_p_input.setValidator(self.float_validator)
        self.ph_h_input.setValidator(self.float_validator)
        self.ph_tol.setValidator(self.float_validator)
        self.ph_t_guess.setValidator(self.float_validator)

        self.uv_u_input.setValidator(self.float_validator)
        self.uv_v_input.setValidator(self.float_validator)
        self.uv_t_guess.setValidator(self.float_validator)
        self.uv_p_guess.setValidator(self.float_validator)

    def get_table_indices(self):
        """
        The table_indices dict has the table row and column names as keys, and the correct index as value.
        This makes it easier to access the correct cell in the table, as there are many rows to keep track of.
        :return: Dictionary containing the table indices
        """
        table_indices = {}
        for i in range(self.table.rowCount()):
            table_indices[self.table.verticalHeaderItem(i).text()] = i

        for i in range(self.table.columnCount()):
            table_indices[self.table.horizontalHeaderItem(i).text()] = i

        return table_indices

    def set_units(self):
        """
        Sets the preferred units into the Units column in the table
        """
        for property, unit in self.units.items():
            row = self.table_indices[property]
            self.table.setItem(row, 0, QTableWidgetItem(unit))

    def set_table_value(self, property, col_name, base_unit, to_unit, value):
        """
        Set-function for setting a value to the table
        :param property: Table row name
        :param col_name: Table column name
        :param value: Value to be stored in the desired cell
        """

        quantity = value * self.ureg(base_unit)
        quantity.ito(to_unit)
        converted_value = quantity.magnitude

        row = self.table_indices[property]
        col = self.table_indices[col_name]
        self.table.setItem(row, col, QTableWidgetItem(str(converted_value)))

    def display_settings(self):
        """
        Shows a window where the current model setup is displayed
        """
        self.settings_window = QTextEdit()

        for key, value in self.settings.items():
            self.settings_window.append(str(key) + ": " + str(value))

        self.settings_window.setWindowTitle("Current settings")
        self.settings_window.setReadOnly(True)
        self.settings_window.setSizePolicy(QSizePolicy(QSizePolicy.Maximum, QSizePolicy.Maximum))
        self.settings_window.setSizePolicy(QSizePolicy(QSizePolicy.Maximum, QSizePolicy.Maximum))

        self.settings_window.show()

    def init_fractions(self):
        """
        Creates input fields for the molar fractions. One for each component. If there is only one component,
        the molar fraction is set to 1.00 and is uneditable.
        """
        components = self.component_data["Names"]
        self.component_data["Fractions"] = [0.00] * len(components)

        for i in range(len(components)):
            component = components[i]
            line_edit = QLineEdit()
            line_edit.setValidator(self.float_validator)
            line_edit.setText("0.00")
            line_edit.setObjectName(component)
            line_edit.editingFinished.connect(lambda comp=component: self.change_fraction(comp))

            if len(components) == 1:
                line_edit.setText("1.00")
                line_edit.setEnabled(False)
                self.component_data["Fractions"][i] = 1.00
            else:
                self.component_data["Fractions"][i] = 0.00

            self.fractions_layout.addRow(components[i], line_edit)

    def change_fraction(self, comp_name):
        """
        Changes the fraction of the given component
        :param comp_name: Name of component of which the molar fraction is changed
        """
        frac_line_edit = self.molar_fractions_box.findChild(QLineEdit, comp_name)
        index = self.component_data["Names"].index(comp_name)
        self.component_data["Fractions"][index] = float(frac_line_edit.text())

    def toggle_initial_guess(self):
        self.ps_guess_frame.setEnabled(self.ps_initial_guess.isChecked())
        self.ph_guess_frame.setEnabled(self.ph_initial_guess.isChecked())
        self.uv_t_guess_frame.setEnabled(self.uv_t_initial_guess.isChecked())
        self.uv_p_guess_frame.setEnabled(self.uv_p_initial_guess.isChecked())

    def calculate(self):
        """
        Calls thermopack's flash functions, and populates the table with the results
        """
        self.table.clearContents()
        self.set_units()
        self.download_csv_btn.setEnabled(True)

        fractions = np.array(self.component_data["Fractions"])
        mole_fraction_sum = np.sum(fractions)

        if abs(mole_fraction_sum - 1.00) > 1e-8:
            msg = MolarFractionsErrorMsg(mole_fraction_sum)
            msg.exec_()
            return
        else:
            # Setting the last mol fraction to 1 - the rest of them, to ensure that the total sum is exactly 1
            fractions[-1] = 1 - np.sum(fractions[:-1])

        init_thermopack(self.tp, self.component_data, self.comp_list_name, self.settings)

        flash_mode = self.flash_mode_selection.currentText()
        self.tp.set_ph_tolerance(float(self.ph_tol.text()))

        if flash_mode == "TP":
            T = float(self.tp_t_input.text())
            P = float(self.tp_p_input.text())

            x, y, beta_vap, beta_liq, phase = self.tp.two_phase_tpflash(T, P, fractions)

        elif flash_mode == "PS":
            P = float(self.ps_p_input.text())
            S = float(self.ps_s_input.text())

            if self.ps_initial_guess.isChecked():
                T = float(self.ps_t_guess.text())
            else:
                T = None

            T, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_psflash(press=P, z=fractions, entropy=S, temp=T)

        elif flash_mode == "PH":
            P = float(self.ph_p_input.text())
            H = float(self.ph_h_input.text())

            if self.ph_initial_guess.isChecked():
                T = float(self.ph_t_guess.text())
            else:
                T = None

            T, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_phflash(press=P, z=fractions, enthalpy=H, temp=T)

        elif flash_mode == "UV":
            U = float(self.uv_u_input.text())
            V = float(self.uv_v_input.text())

            if self.uv_t_initial_guess.isChecked():
                T = float(self.uv_t_guess.text())
            else:
                T = None

            if self.uv_p_initial_guess.isChecked():
                P = float(self.uv_p_guess.text())
            else:
                P = None

            T, P, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_uvflash(z=fractions, specific_energy=U,
                                                                              specific_volume=V, temp=T, press=P)

        else:
            return

        is_vap, is_liq = False, False
        phase_type = self.tp.get_phase_type(phase)
        if phase_type == "TWO_PHASE":
            is_vap, is_liq = True, True
        elif phase_type == "LIQUID":
            is_liq = True
        elif phase_type == "VAPOR":
            is_vap = True

        # TODO: SINGLE => Endre kolonnenavn fra liq og vap til Single (liq og vap er jo basically det samme)
        #  Sette samme verdi i begge kolonner

        # TODO: MINIMUM_GIBBS => Endre kolonnenavn til Minimum Gibbs. Sett samme verdi i begge kolonner

        # TODO: FAKE / SOLID => Popup med melding om at resultatet ikke er kompatibelt med GUI-en enda

        component_indices = [self.tp.getcompindex(comp) for comp in self.component_data["Identities"]]
        molecular_weights = [self.tp.compmoleweight(index) for index in component_indices]

        # TODO: Ikke skriv phase=1 eller phase=2. Ha dem som indekser LIQ = 1, VAP = 2

        if is_liq:
            V_liq, dVdT_liq, dVdn_liq = self.tp.specific_volume(T, P, x, phase=1, dvdt=True, dvdn=True)
            H_liq, dHdT_liq, dHdP_liq, dHdn_liq = self.tp.enthalpy(T, P, x, phase=1, dhdt=True, dhdp=True, dhdn=True)
            S_liq, dSdT_liq, dSdP_liq, dSdn_liq = self.tp.entropy(T, P, x, phase=1, dsdt=True, dsdp=True, dsdn=True)

            sos_liq = self.tp.speed_of_sound(T, P, x, y, fractions, beta_vap, beta_liq, phase=1)

            U_liq = H_liq - P * V_liq
            G_liq = H_liq - T * S_liq

            Cp_liq = dHdT_liq
            # TODO: Calculate isochoric heat capacity. Default value is now set to -1
            Cv_liq = -1

            mol_weight_liq = sum([x[i] * molecular_weights[i] for i in range(len(molecular_weights))])

            self.set_table_value("Temperature", "Liq", "degK", "degK", T)
            self.set_table_value("Pressure", "Liq", "Pa", "Pa", P)
            self.set_table_value("Specific volume", "Liq", "m**3 / mol", "m**3 / mol", V_liq)
            self.set_table_value("Internal energy", "Liq", "J / mol", "J / mol", U_liq)
            self.set_table_value("Enthalpy", "Liq", "J / mol", "J / mol", H_liq)
            self.set_table_value("Entropy", "Liq", "J / (K * mol)", "J / (K * mol)", S_liq)
            self.set_table_value("Gibbs energy", "Liq", "J / mol", "J / mol", G_liq)
            self.set_table_value("Isobar heat capacity", "Liq", "J / (K * mol)", "J / (K * mol)", Cp_liq)
            self.set_table_value("Isochor heat capacity", "Liq", "J / (K * mol)", "J / (K * mol)", Cv_liq)
            self.set_table_value("Speed of sound", "Liq", "m / s", "m / s", sos_liq)
            self.set_table_value("Phase fraction", "Liq", "mol / mol", "mol / mol", beta_liq)
            self.set_table_value("Molecular weight", "Liq", "kg / mol", "kg / mol", mol_weight_liq)

        if is_vap:
            V_vap, dVdT_vap, dVdP_vap, dVdn_vap = self.tp.specific_volume(T, P, x, phase=2, dvdt=True, dvdp=True,
                                                                          dvdn=True)
            H_vap, dHdT_vap, dHdP_vap, dHdn_vap = self.tp.enthalpy(T, P, x, phase=2, dhdt=True, dhdp=True, dhdn=True)
            S_vap, dSdT_vap, dSdP_vap, dSdn_vap = self.tp.entropy(T, P, x, phase=2, dsdt=True, dsdp=True, dsdn=True)
            sos_vap = self.tp.speed_of_sound(T, P, x, y, fractions, beta_vap, beta_liq, phase=2)

            U_vap = H_vap - P * V_vap
            G_vap = H_vap - T * S_vap

            Cp_vap = dHdT_vap
            Cv_vap = -1

            mol_weight_vap = sum([y[i] * molecular_weights[i] for i in range(len(molecular_weights))])

            self.set_table_value("Temperature", "Vap", "degK", "degK", T)
            self.set_table_value("Pressure", "Vap", "Pa", "Pa", P)
            self.set_table_value("Specific volume", "Vap", "m**3 / mol", "m**3 / mol", V_vap)
            self.set_table_value("Internal energy", "Vap", "J / mol", "J / mol", U_vap)
            self.set_table_value("Enthalpy", "Vap", "J / mol", "J / mol", H_vap)
            self.set_table_value("Entropy", "Vap", "J / (K * mol)", "J / (K * mol)", S_vap)
            self.set_table_value("Gibbs energy", "Vap", "J / mol", "J / mol", G_vap)
            self.set_table_value("Isobar heat capacity", "Vap", "J / (K * mol)", "J / (K * mol)", Cp_vap)
            self.set_table_value("Isochor heat capacity", "Vap", "J / (K * mol)", "J / (K * mol)", Cv_vap)
            self.set_table_value("Speed of sound", "Vap", "m / s", "m / s", sos_vap)
            self.set_table_value("Phase fraction", "Vap", "mol / mol", "mol / mol", beta_vap)
            self.set_table_value("Molecular weight", "Vap", "kg / mol", "kg / mol", mol_weight_vap)

        if is_liq and is_vap:
            V_overall = V_vap * beta_vap + V_liq * beta_liq
            U_overall = U_vap * beta_vap + U_liq * beta_liq
            H_overall = H_vap * beta_vap + H_liq * beta_liq
            S_overall = S_vap * beta_vap + S_liq * beta_liq
            G_overall = G_vap * beta_vap + G_liq * beta_liq
            Cp_overall = Cp_vap * beta_vap + Cp_liq * beta_liq
            Cv_overall = Cv_vap * beta_vap + Cv_liq * beta_liq
            sos_overall = sos_vap * beta_vap + sos_liq * beta_liq
            frac_overall = beta_vap + beta_liq
            mol_weight_overall = mol_weight_vap * beta_vap + mol_weight_liq * beta_liq

        elif is_liq:
            V_overall = V_liq
            U_overall = U_liq
            H_overall = H_liq
            S_overall = S_liq
            G_overall = G_liq
            Cp_overall = Cp_liq
            Cv_overall = Cv_liq
            sos_overall = sos_liq
            frac_overall = beta_liq
            mol_weight_overall = mol_weight_liq

        elif is_vap:
            V_overall = V_vap
            U_overall = U_vap
            H_overall = H_vap
            S_overall = S_vap
            G_overall = G_vap
            Cp_overall = Cp_vap
            Cv_overall = Cv_vap
            sos_overall = sos_vap
            frac_overall = beta_vap
            mol_weight_overall = mol_weight_vap

        if is_liq or is_vap:
            self.set_table_value("Temperature", "Overall", "degK", "degK", T)
            self.set_table_value("Pressure", "Overall", "Pa", "Pa", P)
            self.set_table_value("Specific volume", "Overall", "m**3 / mol", "m**3 / mol", V_overall)
            self.set_table_value("Internal energy", "Overall", "J / mol", "J / mol", U_overall)
            self.set_table_value("Enthalpy", "Overall", "J / mol", "J / mol", H_overall)
            self.set_table_value("Entropy", "Overall", "J / (K * mol)", "J / (K * mol)", S_overall)
            self.set_table_value("Gibbs energy", "Overall", "J / mol", "J / mol", G_overall)
            self.set_table_value("Isobar heat capacity", "Overall", "J / (K * mol)", "J / (K * mol)", Cp_overall)
            self.set_table_value("Isochor heat capacity", "Overall", "J / (K * mol)", "J / (K * mol)", Cv_overall)
            self.set_table_value("Speed of sound", "Overall", "m / s", "m / s", sos_overall)
            self.set_table_value("Phase fraction", "Overall", "mol / mol", "mol / mol", frac_overall)
            self.set_table_value("Molecular weight", "Overall", "kg / mol", "kg / mol", mol_weight_overall)

        self.table.resizeColumnsToContents()
        self.table.resizeRowsToContents()

    def export_csv(self):
        """
        Creates and saves a csv file with the table data.
        """
        file_dialog = QFileDialog()
        file_dialog.setWindowTitle('Save File')
        file_dialog.setDirectory(os.getcwd())
        file_dialog.setAcceptMode(QFileDialog.AcceptSave)
        file_dialog.setNameFilter('Csv files (*.csv)')
        file_dialog.setDefaultSuffix('csv')

        if file_dialog.exec_() == QFileDialog.Accepted:
            path = file_dialog.selectedFiles()[0]

            if path:
                with open(path, mode="w", newline='', encoding='utf8') as csv_file:
                    field_names = ["", "Unit", "Vap", "Liq", "Overall"]
                    writer = csv.DictWriter(csv_file, fieldnames=field_names)
                    writer.writeheader()

                    for row in range(self.table.rowCount()):
                        row_data = {}
                        row_data[""] = self.table.verticalHeaderItem(row).text()

                        for col in range(self.table.columnCount()):
                            item = self.table.item(row, col)

                            if item is not None:
                                row_data[field_names[col + 1]] = item.text()
                            else:
                                row_data[field_names[col + 1]] = ''

                        writer.writerow(row_data)
