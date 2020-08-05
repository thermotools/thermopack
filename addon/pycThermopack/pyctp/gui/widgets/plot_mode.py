from PyQt5.QtWidgets import QMainWindow, QRadioButton, QButtonGroup, QMessageBox, QCheckBox, QLineEdit, QLabel, \
    QFileDialog, QActionGroup
from PyQt5.QtGui import QIcon, QKeySequence
from PyQt5.uic import loadUi

from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT

from gui.widgets.mpl_canvas import MplCanvas
from gui.widgets.plot_mode_options import PhaseEnvelopeOptionsWindow, BinaryPXYOptionsWindow, PRhoOptionsWindow, \
    GlobalBinaryOptionsWindow
from gui.widgets.units_dialog import UnitsDialog
from gui.utils import get_thermopack, init_thermopack, save_json_data, FloatValidator, MessageBox

import numpy as np
import csv
import os


# TODO: Handle enheter


class PlotMode(QMainWindow):
    """
    A window where different types of (matplotlib) plots can be shown for a given composition and model setup.
    The user may change initial parameters for the calculations and specify some plotting preferences.
    When a plot is generated, the user may download a csv file containing the x and y data for each plotted line
    """

    def __init__(self, data, json_file, component_list_name, model_settings_name, parent=None):
        super().__init__(parent=parent)

        loadUi("widgets/layouts/plot_mode.ui", self)
        self.setWindowTitle("Thermopack - Plot Mode")
        self.showMaximized()

        self.data = data
        self.json_file = json_file

        self.component_data = self.data["Component lists"][component_list_name]
        self.comp_list_name = component_list_name
        self.settings = self.data["Model setups"][model_settings_name]
        self.units = self.data["Units"]

        self.set_toolbar()

        if self.data["Plotting preferences"]:
            self.plotting_preferences = self.data["Plotting preferences"]
        else:
            self.plotting_preferences = self.init_plotting_preferences()
            self.data["Plotting preferences"] = self.plotting_preferences

        # In case the user wants to reset settings
        self.default_plotting_preferences = self.init_plotting_preferences()

        self.redraw = True
        self.redraw_checkbox.setChecked(self.redraw)

        self.init_plot_modes()

        self.model_btn_group = QButtonGroup(parent=self.model_box)
        self.init_model_options()

        self.init_fractions()

        # Initiating thermopack
        self.tp = get_thermopack(category=self.settings["Model category"])

        # Init function depends on settings
        init_thermopack(self.tp, self.component_data, self.comp_list_name, self.settings)

        self.ph_env_toolbtn.clicked.connect(self.show_ph_env_options)
        self.bin_pxy_toolbtn.clicked.connect(self.show_bin_pxy_options)
        self.p_rho_toolbtn.clicked.connect(self.show_p_rho_options)
        self.global_binary_toolbtn.clicked.connect(self.show_global_binary_options)

        self.plot_type_btn_group.buttonClicked.connect(self.change_plot_type)

        # Setup for plot window
        self.canvas = MplCanvas(self.component_data["Names"], self.plotting_preferences)
        self.mpl_toolbar = NavigationToolbar2QT(self.canvas, self)
        self.mpl_toolbar.hide()
        self.canvas.hide()
        self.plot_layout.addWidget(self.mpl_toolbar)
        self.plot_layout.addWidget(self.canvas)

        self.init_isopleth_btns()
        self.redraw_checkbox.clicked.connect(self.toggle_redraw)
        self.plot_button.clicked.connect(self.plot)
        self.download_csv_btn.clicked.connect(self.export_csv)

    def set_toolbar(self):
        """
        Creates the top toolbar
        """
        # Logo
        logo = QLabel("Thermopack  |  Plot Mode  ")
        logo.setStyleSheet("color: #FF8B06; font: 75 28pt 'Agency FB'; padding: 5px 10px 5px 10px;")

        # Top toolbar
        toolbar = self.addToolBar("Tool bar")
        toolbar.setMovable(False)
        toolbar.actionTriggered.connect(self.handle_toolbar_action)
        toolbar.setStyleSheet("padding: 5px 10px 5px 10px;")
        toolbar.addWidget(logo)
        toolbar.addSeparator()

        action_group = QActionGroup(self)
        if self.json_file:
            action_group.addAction(toolbar.addAction(QIcon("icons/save.png"), "Save"))
            self.action_save = self.file_menu.addAction("Save", self.save_plot_settings, QKeySequence("Ctrl+S"))
            self.action_close = self.file_menu.addAction("Close", self.close, QKeySequence("Ctrl+Q"))

        else:
            self.action_close = self.file_menu.addAction("Close", self.close, QKeySequence("Ctrl+Q"))

        action_group.addAction(toolbar.addAction(QIcon("icons/settings.png"), "Units"))
        self.action_units.triggered.connect(self.open_units_window)

    def handle_toolbar_action(self, action):
        """
        Calls the correct function depending on which tool icon was clicked
        :param action: Type of tool clicked
        """
        action = action.text()
        if action == "Save":
            self.save_plot_settings()
        elif action == "Units":
            self.open_units_window()

    @staticmethod
    def init_plotting_preferences():
        """
        :return: Dictionary for storing plotting preferences and parameters
        """
        return {
            "Phase envelope": {
                "Isopleths": {
                    "Minimum pressure": 100000.0,
                    "Maximum pressure": 15000000.0,
                    "Number of isopleths": 15,
                    "Minimum temperature": 200.0,
                    "Maximum temperature": 500.0,
                    "N max": 50
                },
                "TPV": {
                    "Initial pressure": 100000.0,
                    "Maximum pressure": 15000000.0,
                    "Minimum temperature": None,
                    "Step size": 0.1,
                },
                "Critical": {
                    "Temperature": 0.0,
                    "Volume": 0.0,
                    "Error tolerance": 1.0e-7
                },
                "Plotting": {
                    "Colors": ["#1f77b4", "#ff7f0e", "#ffd2d2", "#d5d3ff"],
                    "Grid on": False,
                    "Title": None,
                    "x label": None,
                    "y label": None
                }
            },
            "Binary pxy": {
                "Calc": {
                    "Temperature": 288.0,
                    "Maximum pressure": 1.5e7,
                    "Minimum pressure": 1.0e5,
                    "Maximum dz": 0.003,
                    "Maximum dlns": 0.01,
                },
                "Plotting": {
                    "Colors": ["#1f77b4", "#ff7f0e", "#ffd2d2", "#d5d3ff"],
                    "Grid on": False,
                    "Title": None,
                    "x label": None,
                    "y label": None
                }
            },
            "Pressure density": {
                "Calc": {
                    "Temperatures": [298.0],
                    "Volume range start": 0.50,
                    "Volume range end": 10.0,
                    "Num points": 100,
                },
                "Critical": {
                    "Temperature": 0.0,
                    "Volume": 0.0,
                    "Error tolerance": 1.0e-7
                },
                "Plotting": {
                    "Grid on": False,
                    "Title": None,
                    "x label": None,
                    "y label": None
                },
                "TPV": {
                    "Initial pressure": 100000.0,
                    "Maximum pressure": 15000000.0,
                    "Minimum temperature": None,
                    "Step size": 0.1
                }
            },
            "Global binary": {
                "Calc": {
                    "Minimum pressure": 1.05e5,
                    "Minimum temperature": 2.0,
                    "Azeotropes": True,
                },
                "Plotting": {
                    "Colors": ["black", "blue", "red", "green"],
                    "Linestyles": ["-", "--", ":", "-."],
                    "Grid on": False,
                    "Title": None,
                    "x label": None,
                    "y label": None
                }
            }
        }

    def init_plot_modes(self):
        """
        Disables some plot options if there are too few or too many components
        """
        if len(self.component_data["Names"]) != 2:
            self.binary_pxy_btn.setEnabled(False)
            self.global_binary_btn.setEnabled(False)

    def init_model_options(self):
        """
        Adds model options to a widget depending on model category
        """
        category = self.settings["Model category"]
        if category in ["Cubic", "CPA"]:
            pr_btn = QRadioButton("PR")
            srk_btn = QRadioButton("SRK")
            self.model_box_layout.addWidget(pr_btn)
            self.model_box_layout.addWidget(srk_btn)
            self.model_btn_group.addButton(pr_btn)
            self.model_btn_group.addButton(srk_btn)

            if self.settings["EOS"] == "PR":
                pr_btn.setChecked(True)
            elif self.settings["EOS"] == "SRK":
                srk_btn.setChecked(True)
            else:
                pass

        elif category == "PC-SAFT":
            # No model options for PC-SAFT
            pass

        elif category == "SAFT-VR Mie":
            self.a1_checkbox = QCheckBox("A1")
            self.a2_checkbox = QCheckBox("A2")
            self.a3_checkbox = QCheckBox("A3")
            self.hard_sphere_checkbox = QCheckBox("Hard sphere")
            self.chain_checkbox = QCheckBox("Chain")
            self.a1_checkbox.setChecked(self.settings["Model options"]["A1"])
            self.a2_checkbox.setChecked(self.settings["Model options"]["A2"])
            self.a3_checkbox.setChecked(self.settings["Model options"]["A3"])
            self.hard_sphere_checkbox.setChecked(self.settings["Model options"]["Hard sphere"])
            self.chain_checkbox.setChecked(self.settings["Model options"]["Chain"])
            self.model_box_layout.addWidget(self.a1_checkbox)
            self.model_box_layout.addWidget(self.a2_checkbox)
            self.model_box_layout.addWidget(self.a3_checkbox)
            self.model_box_layout.addWidget(self.hard_sphere_checkbox)
            self.model_box_layout.addWidget(self.chain_checkbox)

        else:
            self.model_box.setVisible(False)

    def init_fractions(self):
        """
        Adds component fraction widgets to window, depending on how many components are chosen
        """
        components = self.component_data["Names"]
        self.component_data["Fractions"] = [0.00] * len(components)

        float_validator = FloatValidator()

        for i in range(len(components)):
            component = components[i]
            line_edit = QLineEdit()
            line_edit.setValidator(float_validator)
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

    def init_isopleth_btns(self):
        """
        Connects isopleth buttons to a show/hide function in MplCanvas
        """
        self.PT_H_btn.clicked.connect(self.canvas.toggle_isenthalps)
        self.PT_S_btn.clicked.connect(self.canvas.toggle_isentropes)

        self.PH_T_btn.clicked.connect(self.canvas.toggle_isotherms)
        self.PH_S_btn.clicked.connect(self.canvas.toggle_isentropes)

        self.PS_T_btn.clicked.connect(self.canvas.toggle_isotherms)
        self.PS_H_btn.clicked.connect(self.canvas.toggle_isenthalps)

        self.TH_P_btn.clicked.connect(self.canvas.toggle_isobars)
        self.TH_S_btn.clicked.connect(self.canvas.toggle_isentropes)

        self.TS_P_btn.clicked.connect(self.canvas.toggle_isobars)
        self.TS_H_btn.clicked.connect(self.canvas.toggle_isenthalps)

        self.isopleth_btn_stack.hide()

    def change_plot_type(self, btn):
        """
        :param btn: Selected radio button containing the selected plot type
        Enables/disables the different plot and model options depending on plot type
        """
        if btn.text() == "Phase envelope":
            self.ph_env_toolbtn.setEnabled(True)
            self.p_rho_toolbtn.setEnabled(False)
            self.bin_pxy_toolbtn.setEnabled(False)
            self.global_binary_toolbtn.setEnabled(False)

            self.molar_fractions_box.setEnabled(True)
            self.primary_vars_box.setEnabled(True)

        elif btn.text() == "Binary pxy":
            self.ph_env_toolbtn.setEnabled(False)
            self.p_rho_toolbtn.setEnabled(False)
            self.bin_pxy_toolbtn.setEnabled(True)
            self.global_binary_toolbtn.setEnabled(False)

            self.molar_fractions_box.setEnabled(False)
            self.primary_vars_box.setEnabled(False)

        elif btn.text() == "Pressure density":
            self.ph_env_toolbtn.setEnabled(False)
            self.p_rho_toolbtn.setEnabled(True)
            self.bin_pxy_toolbtn.setEnabled(False)
            self.global_binary_toolbtn.setEnabled(False)

            self.molar_fractions_box.setEnabled(True)
            self.primary_vars_box.setEnabled(False)

        elif btn.text() == "Global binary":
            self.ph_env_toolbtn.setEnabled(False)
            self.p_rho_toolbtn.setEnabled(False)
            self.bin_pxy_toolbtn.setEnabled(False)
            self.global_binary_toolbtn.setEnabled(True)

            self.molar_fractions_box.setEnabled(False)
            self.primary_vars_box.setEnabled(False)

        else:
            pass

    def show_ph_env_options(self):
        """
        Opens an option window where initial parameters for phase envelope plot can be set
        """
        options_window = PhaseEnvelopeOptionsWindow(self.plotting_preferences, self.default_plotting_preferences)
        options_window.exec_()

    def show_bin_pxy_options(self):
        """
        Opens an option window where initial parameters for binary pxy plot can be set
        """
        options_window = BinaryPXYOptionsWindow(self.plotting_preferences, self.default_plotting_preferences)
        options_window.exec_()

    def show_p_rho_options(self):
        """
        Opens an option window where initial parameters for pressure density plot can be set
        """
        options_window = PRhoOptionsWindow(self.plotting_preferences, self.default_plotting_preferences)
        options_window.exec_()

    def show_global_binary_options(self):
        """
        Opens an option window where initial parameters for global binary plot can be set
        """
        options_window = GlobalBinaryOptionsWindow(self.plotting_preferences, self.default_plotting_preferences)
        options_window.exec_()

    def change_fraction(self, comp_name):
        """
        Changes the mole fraction of a component
        :param str comp_name: Name of the component
        """
        line_edit = self.molar_fractions_box.findChild(QLineEdit, comp_name)
        mol_frac = line_edit.text().replace(",", ".")
        index = self.component_data["Names"].index(comp_name)

        self.component_data["Fractions"][index] = float(mol_frac)

    def toggle_redraw(self, is_checked):
        self.redraw = is_checked

    def plot(self):
        """
        Checks type of plot selected, gets the correct parameters, inits thermopack,
        and calls the correct plot function in MplCanvas
        """
        category = self.settings["Model category"]
        plot_type = self.plot_type_btn_group.checkedButton().text()
        prim_vars = self.prim_vars_dropdown.currentText()

        if category in ["Cubic", "CPA"]:
            eos = self.model_btn_group.checkedButton().text()
            if self.settings["EOS"] != eos:
                self.settings["EOS"] = eos

        elif category == "SAFT-VR Mie":
            self.settings["Model options"]["A1"] = self.a1_checkbox.isChecked()
            self.settings["Model options"]["A2"] = self.a2_checkbox.isChecked()
            self.settings["Model options"]["A3"] = self.a3_checkbox.isChecked()
            self.settings["Model options"]["Hard sphere"] = self.hard_sphere_checkbox.isChecked()
            self.settings["Model options"]["Chain"] = self.chain_checkbox.isChecked()

        init_thermopack(self.tp, self.component_data, self.comp_list_name, self.settings)

        fractions = np.array(self.component_data["Fractions"])

        if self.canvas.empty:
            self.canvas.axes = self.canvas.fig.add_subplot(111)
            self.canvas.empty = False

        if self.redraw:
            self.canvas.axes.cla()

        self.isopleth_btn_stack.hide()
        self.download_csv_btn.setEnabled(True)

        if plot_type in ["Phase envelope", "Pressure density"]:
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

        if plot_type == "Phase envelope":
            self.canvas.plot_envelope(self.tp, prim_vars, fractions)
            self.canvas.show()
            self.mpl_toolbar.show()
            if self.plotting_preferences["Phase envelope"]["Isopleths"]["Number of isopleths"] > 0:
                self.isopleth_btn_stack.show()

        elif plot_type == "Binary pxy":
            self.canvas.plot_binary_pxy(self.tp)
            self.canvas.show()
            self.mpl_toolbar.show()

        elif plot_type == "Pressure density":
            self.canvas.plot_pressure_density(self.tp, fractions)
            self.canvas.show()
            self.mpl_toolbar.show()

        elif plot_type == "Global binary":
            self.canvas.plot_global_binary(self.tp)
            self.canvas.show()
            self.mpl_toolbar.show()

        else:
            pass

    def export_csv(self):
        """
        Creates and saves a csv file with the (x,y) data from all the currently plotted lines
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

                    writer = csv.writer(csv_file)
                    lines = self.canvas.axes.lines

                    for i in range(len(lines)):
                        line = lines[i]
                        x_list = [line.get_label() + " x"]
                        y_list = [line.get_label() + " y"]

                        x_list += list(line.get_xdata())
                        y_list += list(line.get_ydata())

                        writer.writerows([x_list, y_list])

    def open_units_window(self):
        self.dialog = UnitsDialog(self.units)
        self.dialog.show()

    def save_plot_settings(self):
        """
        Saves data to the current JSON-file.
        """
        if self.json_file:
            save_json_data(self.data, self.json_file)
            self.msg = MessageBox("Success", "Data saved to %s." % self.json_file)
        else:
            self.msg = MessageBox("Failed", "Could not save data. No file is chosen.")

        self.msg.exec()
