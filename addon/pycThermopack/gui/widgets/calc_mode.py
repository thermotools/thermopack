from PyQt5.QtWidgets import QMainWindow, QTableWidgetItem, QFileDialog, QTextEdit, QSizePolicy, QLineEdit, QLabel, \
    QActionGroup
from PyQt5.QtGui import QIcon
from PyQt5.uic import loadUi

import csv
import os

import numpy as np
import pint

from gui.widgets.units_dialog import UnitsDialog
from gui.utils import get_thermopack, init_thermopack, FloatValidator, MessageBox


class CalcMode(QMainWindow):
    """
    Calculation mode: Own window to calculate, display and save flash data
    """

    def __init__(self, data, json_file, component_list_name, model_settings_name, parent=None):
        super().__init__(parent=parent)

        loadUi("gui/layouts/calc_mode.ui", self)
        self.setWindowTitle("Thermopack - Calculation Mode")
        self.showMaximized()

        self.set_toolbar()

        self.data = data
        self.json_file = json_file

        self.component_data = self.data["Component lists"][component_list_name]
        self.comp_list_name = component_list_name
        self.settings = self.data["Model setups"][model_settings_name]
        self.units_data = self.data["Units"]

        self.tp = get_thermopack(category=self.settings["Model category"])

        self.input_stack_indices = {
            "TP": 0,
            "PS": 1,
            "PH": 2,
            "UV": 3
        }

        self.set_units()
        self.set_label_units()

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
        action_group.addAction(toolbar.addAction(QIcon("gui/icons/settings.png"), "Units"))

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
        self.dialog.units_changed.connect(self.update_units)
        self.dialog.show()

    def show_input_params(self, flash_mode="TP"):
        """
        Shows the correct input fields, depending on type of flash chosen
        :param flash_mode: Type of flash (TP, PS, PH, UV)
        """
        index = self.input_stack_indices[flash_mode]
        self.flash_input_stack.setCurrentIndex(index)

    def set_units(self):
        units = self.units_data["Selected"]

        self.units = {
            "Temperature": units["Temperature"],
            "Pressure": units["Pressure"],
            "Volume": units["Volume"],
            "Specific volume": "%s / %s" % (units["Volume"], units["Amount"]),
            "Internal energy": "%s / %s" % (units["Energy"], units["Amount"]),
            "Enthalpy": "%s / %s" % (units["Energy"], units["Amount"]),
            "Entropy": "%s / (%s * %s)" % (units["Energy"], units["Temperature"], units["Amount"]),
            "Gibbs energy": "%s / %s" % (units["Energy"], units["Amount"]),
            "Isobar heat capacity": "%s / (%s * %s)" % (units["Energy"], units["Temperature"], units["Amount"]),
            "Isochor heat capacity": "%s / (%s * %s)" % (units["Energy"], units["Temperature"], units["Amount"]),
            "Speed of sound": "%s" % (units["Speed"]),
            "Molecular weight": "%s / %s" % ("kg", units["Amount"]),
            "Phase fraction": "%s / %s" % (units["Amount"], units["Amount"]),
        }

    def set_label_units(self):
        self.tp_t_label.setText(
            self.tp_t_label.text()[:self.tp_t_label.text().find(' [')] + " [%s]" % self.units["Temperature"])
        self.tp_p_label.setText(
            self.tp_p_label.text()[:self.tp_p_label.text().find(' [')] + " [%s]" % self.units["Pressure"])

        self.ps_p_label.setText(
            self.ps_p_label.text()[:self.ps_p_label.text().find(' [')] + " [%s]" % self.units["Pressure"])
        self.ps_s_label.setText(
            self.ps_s_label.text()[:self.ps_s_label.text().find(' [')] + " [%s]" % self.units["Entropy"])
        self.ps_t_guess_label.setText(
            self.ps_t_guess_label.text()[:self.ps_t_guess_label.text().find(' [')] +
            " [%s]" % self.units["Temperature"])

        self.ph_p_label.setText(
            self.ph_p_label.text()[:self.ph_p_label.text().find(' [')] + " [%s]" % self.units["Pressure"])
        self.ph_h_label.setText(
            self.ph_h_label.text()[:self.ph_h_label.text().find(' [')] + " [%s]" % self.units["Enthalpy"])
        self.ph_t_guess_label.setText(
            self.ph_t_guess_label.text()[:self.ph_t_guess_label.text().find(' [')] +
            " [%s]" % self.units["Temperature"])

        self.uv_u_label.setText(
            self.uv_u_label.text()[:self.uv_u_label.text().find(' [')] + " [%s]" % self.units["Internal energy"])
        self.uv_v_label.setText(
            self.uv_v_label.text()[:self.uv_v_label.text().find(' [')] + " [%s]" % self.units["Volume"])
        self.uv_t_guess_label.setText(
            self.uv_t_guess_label.text()[:self.uv_t_guess_label.text().find(' [')] +
            " [%s]" % self.units["Temperature"])
        self.uv_p_guess_label.setText(
            self.uv_p_guess_label.text()[:self.uv_p_guess_label.text().find(' [')] +
            " [%s]" % self.units["Pressure"])

    def set_input_placeholders(self):
        """
        Sets placeholders for temperature and pressure input fields to standard temperature and pressure
        """
        std_temp = self.ureg.Quantity(298.0, "degK")
        T = str(std_temp.to(self.units["Temperature"]).magnitude)

        std_press = self.ureg.Quantity(100000, "Pa")
        P = str(std_press.to(self.units["Pressure"]).magnitude)

        self.tp_t_input.setText(T)
        self.tp_p_input.setText(P)
        self.ps_p_input.setText(P)
        self.ps_t_guess.setText(T)
        self.ph_p_input.setText(P)
        self.ph_t_guess.setText(T)
        self.uv_t_guess.setText(T)
        self.uv_p_guess.setText(P)

    def update_units(self):
        self.set_units()
        self.set_label_units()
        self.set_input_placeholders()

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

    def set_table_units(self):
        """
        Sets the preferred units into the Units column in the table
        """
        for property, unit in self.units.items():
            try:
                row = self.table_indices[property]
                self.table.setItem(row, 0, QTableWidgetItem(unit))
            except KeyError:
                pass

    def set_table_value(self, property, col_name, base_unit, to_unit, value):
        """
        Set-function for setting a value to the table
        :param property: Table row name
        :param col_name: Table column name
        :param value: Value to be stored in the desired cell
        """

        quantity = self.ureg.Quantity(value, base_unit)
        converted_value = quantity.to(to_unit).magnitude

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
        fractions = np.array(self.component_data["Fractions"])
        mole_fraction_sum = np.sum(fractions)

        if abs(mole_fraction_sum - 1.00) > 1e-8:
            msg_title = "Molar fractions error"
            msg_text = "Molar fractions have to add up to 1.00. Currently the sum is %s." % mole_fraction_sum
            msg = MessageBox(msg_title, msg_text)
            msg.exec_()
            return
        else:
            # Setting the last mol fraction to 1 - the rest of them, to ensure that the total sum is exactly 1
            fractions[-1] = 1 - np.sum(fractions[:-1])

        self.table.clearContents()
        self.set_table_units()

        init_thermopack(self.tp, self.component_data, self.comp_list_name, self.settings)

        flash_mode = self.flash_mode_selection.currentText()
        self.tp.set_ph_tolerance(float(self.ph_tol.text()))

        if flash_mode == "TP":
            T = float(self.tp_t_input.text())
            P = float(self.tp_p_input.text())

            # Conversion to standard SI to be used in functions
            temp = self.ureg.Quantity(T, self.units["Temperature"])
            T = temp.to("degK").magnitude

            press = self.ureg.Quantity(P, self.units["Pressure"])
            P = press.to("Pa").magnitude

            # TODO: Need an exception thrown in two_phase_tpflash() to be caught in case calculation fails
            x, y, beta_vap, beta_liq, phase = self.tp.two_phase_tpflash(T, P, fractions)

        elif flash_mode == "PS":
            P = float(self.ps_p_input.text())
            S = float(self.ps_s_input.text())

            if self.ps_initial_guess.isChecked():
                T = float(self.ps_t_guess.text())
            else:
                T = None

            try:
                T, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_psflash(press=P, z=fractions, entropy=S, temp=T)
            except Exception as error:
                msg = MessageBox("Error", str(error))
                msg.exec_()
                return

        elif flash_mode == "PH":
            P = float(self.ph_p_input.text())
            H = float(self.ph_h_input.text())

            if self.ph_initial_guess.isChecked():
                T = float(self.ph_t_guess.text())
            else:
                T = None

            try:
                T, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_phflash(press=P, z=fractions, enthalpy=H, temp=T)
            except Exception as error:
                msg = MessageBox("Error", str(error))
                msg.exec_()
                return

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

            # TODO: Need an exception thrown in two_phase_uvflash() to be caught in case calculation fails
            T, P, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_uvflash(z=fractions, specific_energy=U,
                                                                              specific_volume=V, temp=T, press=P)

        else:
            return

        phase_type = self.tp.get_phase_type(phase)
        LIQUID, VAPOR = 1, 2

        is_liq, is_vap = True, True

        if phase_type == "TWO_PHASE":
            self.table.horizontalHeaderItem(1).setText("Vapor")
            self.table.horizontalHeaderItem(2).setText("Liquid")
            pass

        elif phase_type == "LIQUID":
            is_vap = False
            self.table.horizontalHeaderItem(1).setText("Vapor")
            self.table.horizontalHeaderItem(2).setText("Liquid")

        elif phase_type == "VAPOR":
            is_liq = False
            self.table.horizontalHeaderItem(1).setText("Vapor")
            self.table.horizontalHeaderItem(2).setText("Liquid")

        elif phase_type == "SINGLE":
            self.table.horizontalHeaderItem(1).setText("Single")
            self.table.horizontalHeaderItem(2).setText("Single")
            pass

        elif phase_type == "MINIMUM_GIBBS":
            self.table.horizontalHeaderItem(1).setText("Minimum Gibbs")
            self.table.horizontalHeaderItem(2).setText("Minimum Gibbs")
            pass

        elif phase_type == "FAKE":
            msg = MessageBox("Error", "Currently no functionality for phase " + phase_type)
            msg.exec_()

        elif phase_type == "SOLID":
            msg = MessageBox("Error", "Currently no functionality for phase " + phase_type)
            msg.exec_()

        else:
            return

        component_indices = [self.tp.getcompindex(comp) for comp in self.component_data["Identities"]]
        molecular_weights = [self.tp.compmoleweight(index) for index in component_indices]

        if is_liq:
            V_liq, dVdT_liq, dVdn_liq = self.tp.specific_volume(T, P, x, phase=LIQUID, dvdt=True, dvdn=True)
            H_liq, dHdT_liq, dHdP_liq, dHdn_liq = self.tp.enthalpy(T, P, x, phase=LIQUID, dhdt=True, dhdp=True,
                                                                   dhdn=True)
            S_liq, dSdT_liq, dSdP_liq, dSdn_liq = self.tp.entropy(T, P, x, phase=LIQUID, dsdt=True, dsdp=True,
                                                                  dsdn=True)
            U_liq, dUdT_liq, dUdV_liq = self.tp.internal_energy_tv(T, V_liq, x, dedt=True, dedv=True)

            sos_liq = self.tp.speed_of_sound(T, P, x, y, fractions, beta_vap, beta_liq, phase=LIQUID)

            U_liq = H_liq - P * V_liq
            G_liq = H_liq - T * S_liq

            Cp_liq = dHdT_liq
            Cv_liq = dUdT_liq

            mol_weight_liq = sum([x[i] * molecular_weights[i] for i in range(len(molecular_weights))])

            self.set_table_value("Temperature", "Liq", "degK", self.units["Temperature"], T)
            self.set_table_value("Pressure", "Liq", "Pa", self.units["Pressure"], P)
            self.set_table_value("Specific volume", "Liq", "m ** 3 / mol", self.units["Specific volume"], V_liq)
            self.set_table_value("Internal energy", "Liq", "J / mol", self.units["Internal energy"], U_liq)
            self.set_table_value("Enthalpy", "Liq", "J / mol", self.units["Enthalpy"], H_liq)
            self.set_table_value("Entropy", "Liq", "J / (K * mol)", self.units["Entropy"], S_liq)
            self.set_table_value("Gibbs energy", "Liq", "J / mol", self.units["Gibbs energy"], G_liq)
            self.set_table_value("Isobar heat capacity", "Liq", "J / (K * mol)",
                                 self.units["Isobar heat capacity"], Cp_liq)
            self.set_table_value("Isochor heat capacity", "Liq", "J / (K * mol)",
                                 self.units["Isochor heat capacity"], Cv_liq)
            self.set_table_value("Speed of sound", "Liq", "m / s", self.units["Speed of sound"], sos_liq)
            self.set_table_value("Phase fraction", "Liq", "mol / mol", self.units["Phase fraction"], beta_liq)
            self.set_table_value("Molecular weight", "Liq", "kg / mol", self.units["Molecular weight"], mol_weight_liq)

        if is_vap:
            V_vap, dVdT_vap, dVdP_vap, dVdn_vap = self.tp.specific_volume(T, P, x, phase=VAPOR, dvdt=True, dvdp=True,
                                                                          dvdn=True)
            H_vap, dHdT_vap, dHdP_vap, dHdn_vap = self.tp.enthalpy(T, P, x, phase=VAPOR, dhdt=True, dhdp=True,
                                                                   dhdn=True)
            S_vap, dSdT_vap, dSdP_vap, dSdn_vap = self.tp.entropy(T, P, x, phase=VAPOR, dsdt=True, dsdp=True, dsdn=True)
            U_vap, dUdT_vap, dUdV_vap = self.tp.internal_energy_tv(T, V_vap, x, dedt=True, dedv=True)
            sos_vap = self.tp.speed_of_sound(T, P, x, y, fractions, beta_vap, beta_liq, phase=VAPOR)

            U_vap = H_vap - P * V_vap
            G_vap = H_vap - T * S_vap

            Cp_vap = dHdT_vap
            Cv_vap = dUdT_vap

            mol_weight_vap = sum([y[i] * molecular_weights[i] for i in range(len(molecular_weights))])

            self.set_table_value("Temperature", "Vap", "degK", self.units["Temperature"], T)
            self.set_table_value("Pressure", "Vap", "Pa", self.units["Pressure"], P)
            self.set_table_value("Specific volume", "Vap", "m**3 / mol", self.units["Specific volume"], V_vap)
            self.set_table_value("Internal energy", "Vap", "J / mol", self.units["Internal energy"], U_vap)
            self.set_table_value("Enthalpy", "Vap", "J / mol", self.units["Enthalpy"], H_vap)
            self.set_table_value("Entropy", "Vap", "J / (K * mol)", self.units["Entropy"], S_vap)
            self.set_table_value("Gibbs energy", "Vap", "J / mol", self.units["Gibbs energy"], G_vap)
            self.set_table_value("Isobar heat capacity", "Vap", "J / (K * mol)",
                                 self.units["Isobar heat capacity"], Cp_vap)
            self.set_table_value("Isochor heat capacity", "Vap", "J / (K * mol)",
                                 self.units["Isochor heat capacity"], Cv_vap)
            self.set_table_value("Speed of sound", "Vap", "m / s", self.units["Speed of sound"], sos_vap)
            self.set_table_value("Phase fraction", "Vap", "mol / mol", self.units["Phase fraction"], beta_vap)
            self.set_table_value("Molecular weight", "Vap", "kg / mol", self.units["Molecular weight"], mol_weight_vap)

        if is_liq and is_vap:

            if beta_vap == -1 and beta_liq == -1:
                beta_vap, beta_liq = 0.5, 0.5

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
            self.set_table_value("Temperature", "Overall", "degK", self.units["Temperature"], T)
            self.set_table_value("Pressure", "Overall", "Pa", self.units["Pressure"], P)
            self.set_table_value("Specific volume", "Overall", "m**3 / mol", self.units["Specific volume"], V_overall)
            self.set_table_value("Internal energy", "Overall", "J / mol", self.units["Internal energy"], U_overall)
            self.set_table_value("Enthalpy", "Overall", "J / mol", self.units["Enthalpy"], H_overall)
            self.set_table_value("Entropy", "Overall", "J / (K * mol)", self.units["Entropy"], S_overall)
            self.set_table_value("Gibbs energy", "Overall", "J / mol", self.units["Gibbs energy"], G_overall)
            self.set_table_value("Isobar heat capacity", "Overall", "J / (K * mol)",
                                 self.units["Isobar heat capacity"], Cp_overall)
            self.set_table_value("Isochor heat capacity", "Overall", "J / (K * mol)",
                                 self.units["Isochor heat capacity"], Cv_overall)
            self.set_table_value("Speed of sound", "Overall", "m / s", self.units["Speed of sound"], sos_overall)
            self.set_table_value("Phase fraction", "Overall", "mol / mol", self.units["Phase fraction"], frac_overall)
            self.set_table_value("Molecular weight", "Overall", "kg / mol",
                                 self.units["Molecular weight"], mol_weight_overall)

        self.table.resizeColumnsToContents()
        self.table.resizeRowsToContents()

        self.download_csv_btn.setEnabled(True)

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
