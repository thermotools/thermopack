from PyQt5.QtWidgets import QMainWindow, QDoubleSpinBox, QTableWidgetItem, QFileDialog, QTextEdit, QSizePolicy
from PyQt5.uic import loadUi

import numpy as np
import csv
import os

from cpa import cpa
from cubic import cubic
from gui.widgets.plot_mode import MolarFractionsErrorMsg
from pcsaft import pcsaft
from saftvrmie import saftvrmie

# TODO: Validators for molar fractions (endre til QLineEdits)


class CalcMode(QMainWindow):
    """
    Calculation mode: Own window to calculate, display and save flash data
    """
    def __init__(self, component_data, settings, parent=None):
        super().__init__(parent=parent)

        loadUi("widgets/layouts/calc_mode.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.component_data = component_data
        self.settings = settings
        self.tp = self.get_thermopack()

        self.input_stack_indices = {
            "TP": 0,
            "PS": 1,
            "PH": 2,
            "UV": 3
        }

        self.units = {
            "Temperature": "K",
            "Pressure": "Pa",
            "Specific volume": "m^3/mol",
            "Internal energy": "J/mol",
            "Enthalpy": "J/mol",
            "Entropy": "J/(K mol)",
            "Gibbs energy": "J/mol",
            "Isobar heat capacity": "J/(K mol)",
            "Isochor heat capacity": "J/(K mol)",
            "Speed of sound": "m/s",
            "Molecular weight": "kg/mol",
            "Phase fraction": "mol/mol"
        }

        self.table_indices = self.get_table_indices()

        self.show_input_params()
        self.init_fractions()

        self.flash_mode_selection.currentTextChanged.connect(self.show_input_params)
        self.calculate_btn.clicked.connect(self.calculate)
        self.download_csv_btn.clicked.connect(self.export_csv)

        self.action_display_settings.triggered.connect(self.display_settings)

        self.ps_initial_guess.stateChanged.connect(self.toggle_initial_guess)
        self.ph_initial_guess.stateChanged.connect(self.toggle_initial_guess)
        self.uv_t_initial_guess.stateChanged.connect(self.toggle_initial_guess)
        self.uv_p_initial_guess.stateChanged.connect(self.toggle_initial_guess)

    def get_thermopack(self):
        """
        Returns the correct type of thermopack instance depending on the chosen model
        :return: Thermopack instance
        """
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

    def init_thermopack(self):
        """
        Initiates thermopack and sets the chosen model parameters
        """
        comps = ",".join(self.component_data["Identities"])
        model_ref = self.settings["Model options"]["Reference"]

        category = self.settings["Model category"]
        if category in ["Cubic", "CPA"]:
            eos = self.settings["EOS"]
            mixing = self.settings["Model options"]["Mixing rule"]
            alpha = self.settings["Model options"]["Alpha correlation"]

            self.tp.init(comps=comps, eos=eos, mixing=mixing, alpha=alpha, parameter_reference=model_ref)

        elif category == "PC-SAFT":
            self.tp.init(comps=comps, parameter_reference=model_ref)

        elif category == "SAFT-VR Mie":
            self.tp.init(comps=comps, parameter_reference=model_ref)
            # TODO: Set a1, a2, a3, hard sphere, chain

    def show_input_params(self, flash_mode="TP"):
        """
        Shows the correct input fields, depending on type of flash chosen
        :param flash_mode: Type of flash (TP, PS, PH, UV)
        """
        index = self.input_stack_indices[flash_mode]
        self.flash_input_stack.setCurrentIndex(index)

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

    def set_value(self, property, col_name, value):
        """
        Set-function for setting a value to the table
        :param property: Table row name
        :param col_name: Table column name
        :param value: Value to be stored in the desired cell
        """
        row = self.table_indices[property]
        col = self.table_indices[col_name]
        self.table.setItem(row, col, QTableWidgetItem(str(value)))

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
        # TODO: Turn this into a LineEdit
        """
        Creates input fields for the molar fractions. One for each component. If there is only one component,
        the molar fraction is set to 1.00 and is uneditable.
        """
        components = self.component_data["Names"]
        self.component_data["Fractions"] = [0.00] * len(components)

        for i in range(len(components)):
            spin_box = QDoubleSpinBox()
            spin_box.setMinimumWidth(60)
            spin_box.setMaximum(1.00)
            spin_box.setSingleStep(0.10)

            spin_box.valueChanged.connect(lambda value, x=components[i]: self.change_fraction(value, x))

            if len(components) == 1:
                spin_box.setValue(1.00)
                spin_box.setEnabled(False)
                self.component_data["Fractions"][i] = 1.00
            else:
                self.component_data["Fractions"][i] = 0.00

            self.fractions_layout.addRow(components[i], spin_box)

    def change_fraction(self, value, comp_name):
        """
        Changes the fraction of the given component
        :param value: New molar fraction
        :param comp_name: Name of component of which the molar fraction is changed
        """
        index = self.component_data["Names"].index(comp_name)
        self.component_data["Fractions"][index] = value

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

        if mole_fraction_sum > 1.000001 or mole_fraction_sum < 0.999999:
            # TODO: Q: Dette er litt tricky med floating point numbers. Hvor nøyaktig må det være?
            msg = MolarFractionsErrorMsg(mole_fraction_sum)
            msg.exec_()
            return

        self.init_thermopack()

        flash_mode = self.flash_mode_selection.currentText()
        self.tp.set_ph_tolerance(1.0e-8)  # Skal denne kunne endres?

        # TODO: Q: Trenger en måte å handle errors uten at hele programmet krasjer

        if flash_mode == "TP":
            T = float(self.tp_t_input.valueFromText(self.tp_t_input.cleanText()))
            P = float(self.tp_p_input.valueFromText(self.tp_p_input.cleanText()))

            x, y, beta_vap, beta_liq, phase = self.tp.two_phase_tpflash(T, P, fractions)

        elif flash_mode == "PS":
            P = float(self.ps_p_input.valueFromText(self.ps_p_input.cleanText()))
            S = float(self.ps_s_input.valueFromText(self.ps_s_input.cleanText()))

            if self.ps_initial_guess.isChecked():
                T = float(self.ps_t_guess.valueFromText(self.ps_t_guess.cleanText()))
            else:
                T = None

            T, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_psflash(press=P, z=fractions, entropy=S, temp=T)

        elif flash_mode == "PH":
            P = float(self.ph_p_input.valueFromText(self.ph_p_input.cleanText()))
            H = float(self.ph_h_input.valueFromText(self.ph_h_input.cleanText()))

            if self.ph_initial_guess.isChecked():
                T = float(self.ph_t_guess.valueFromText(self.ph_t_guess.cleanText()))
            else:
                T = None

            T, x, y, beta_vap, beta_liq, phase = self.tp.two_phase_phflash(press=P, z=fractions, enthalpy=H, temp=T)

        elif flash_mode == "UV":
            U = float(self.uv_u_input.valueFromText(self.uv_u_input.cleanText()))
            V = float(self.uv_v_input.valueFromText(self.uv_v_input.cleanText()))

            if self.uv_t_initial_guess.isChecked():
                T = float(self.uv_t_guess.valueFromText(self.uv_t_guess.cleanText()))
            else:
                T = None

            if self.uv_p_initial_guess.isChecked():
                P = float(self.uv_p_guess.valueFromText(self.uv_p_guess.cleanText()))
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

        # TODO: Q: Hva betyr de andre i_phase-ene MINIMUM_GIBBS, SINGLE, SOLID, FAKE?

        # TODO: Q: phase: Liq = 1, Vap = 2. Stemmer dette alltid?
        component_indices = [self.tp.getcompindex(comp) for comp in self.component_data["Identities"]]
        molecular_weights = [self.tp.compmoleweight(index) for index in component_indices]

        if is_liq:
            V_liq, dVdT_liq, dVdn_liq = self.tp.specific_volume(T, P, x, phase=1, dvdt=True, dvdn=True)
            H_liq, dHdT_liq, dHdP_liq, dHdn_liq = self.tp.enthalpy(T, P, x, phase=1, dhdt=True, dhdp=True, dhdn=True)
            S_liq, dSdT_liq, dSdP_liq, dSdn_liq = self.tp.entropy(T, P, x, phase=1, dsdt=True, dsdp=True, dsdn=True)

            # TODO: Q: Hva skal phase være her? Får veldig høyt tall for vann (3632 mot 1840)
            sos_liq = self.tp.speed_of_sound(T, P, x, y, fractions, beta_vap, beta_liq, phase=1)

            U_liq = H_liq - P * V_liq
            G_liq = H_liq - T * S_liq

            Cp_liq = dHdT_liq
            Cv_liq = -1  # TODO: Q: Hvordan regne ut denne?

            mol_weight_liq = sum([x[i] * molecular_weights[i] for i in range(len(molecular_weights))])

            self.set_value("Temperature", "Liq", T)
            self.set_value("Pressure", "Liq", P)
            self.set_value("Specific volume", "Liq", V_liq)
            self.set_value("Internal energy", "Liq", U_liq)
            self.set_value("Enthalpy", "Liq", H_liq)
            self.set_value("Entropy", "Liq", S_liq)
            self.set_value("Gibbs energy", "Liq", G_liq)
            self.set_value("Isobar heat capacity", "Liq", Cp_liq)
            self.set_value("Isochor heat capacity", "Liq", Cv_liq)
            self.set_value("Speed of sound", "Liq", sos_liq)
            self.set_value("Phase fraction", "Liq", beta_liq)
            self.set_value("Molecular weight", "Liq", mol_weight_liq)

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

            self.set_value("Temperature", "Vap", T)
            self.set_value("Pressure", "Vap", P)
            self.set_value("Specific volume", "Vap", V_vap)
            self.set_value("Internal energy", "Vap", U_vap)
            self.set_value("Enthalpy", "Vap", H_vap)
            self.set_value("Entropy", "Vap", S_vap)
            self.set_value("Gibbs energy", "Vap", G_vap)
            self.set_value("Isobar heat capacity", "Vap", Cp_vap)
            self.set_value("Isochor heat capacity", "Vap", Cv_vap)
            self.set_value("Speed of sound", "Vap", sos_vap)
            self.set_value("Phase fraction", "Vap", beta_vap)
            self.set_value("Molecular weight", "Vap", mol_weight_vap)

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
            # TODO: Q: T og P er vel alltid fast siden det er snakk om likevekt?
            self.set_value("Temperature", "Overall", T)
            self.set_value("Pressure", "Overall", P)
            self.set_value("Specific volume", "Overall", V_overall)
            self.set_value("Internal energy", "Overall", U_overall)
            self.set_value("Enthalpy", "Overall", H_overall)
            self.set_value("Entropy", "Overall", S_overall)
            self.set_value("Gibbs energy", "Overall", G_overall)
            self.set_value("Isobar heat capacity", "Overall", Cp_overall)
            self.set_value("Isochor heat capacity", "Overall", Cv_overall)
            self.set_value("Speed of sound", "Overall", sos_overall)
            self.set_value("Phase fraction", "Overall", frac_overall)
            self.set_value("Molecular weight", "Overall", mol_weight_overall)

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
