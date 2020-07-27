from PyQt5.QtWidgets import QMainWindow, QRadioButton, QButtonGroup, QDoubleSpinBox, QColorDialog, QMessageBox, QDialog, \
    QCheckBox
from PyQt5.uic import loadUi

from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT

from gui.widgets.mpl_canvas import MplCanvas

import numpy as np

from cubic import cubic
from cpa import cpa
from pcsaft import pcsaft
from saftvrmie import saftvrmie


# TODO: Mulighet for å plotte flere ting oppå hverandre (Default: Clear hver gang, mulig å endre eks checkbox)

# TODO: Når isopleter toggles on/off, fjern/legg til i legend

# TODO: Lagre paramtere i self.plot_settings = {}

# TODO: Velge å regne ut isopleter eller ikke. Tar en stund (Sjekkboks)

# TODO: Mulighet for å lagre (x, y)-data som csv

# TODO: Ikke ha toolbtn-boks med eget vindu. Ha stack som bestemmer hvilke options. Én for hver plotteopsjon


class PlotMode(QMainWindow):
    """
    A window where different types of (matplotlib) plots can be shown for a given composition and model setup.
    The user may change initial parameters for the calculations and specify some plotting preferences.
    When a plot is generated, the user may download a csv file containing the (x, y) data.
    """
    def __init__(self, component_data, settings, parent=None):
        super().__init__(parent=parent)

        loadUi("widgets/layouts/plot_mode.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.component_data = component_data
        self.settings = settings

        self.init_plot_modes()

        self.model_btn_group = QButtonGroup(parent=self.model_box)
        self.init_model_options()

        self.init_fractions()

        # Initiating thermopack
        self.tp = self.get_thermopack()

        # Init function depends on settings
        self.init_tp()

        self.line_color_tool_btn.clicked.connect(self.pick_line_color)
        self.point_color_tool_btn.clicked.connect(self.pick_point_color)
        self.ph_env_toolbtn.clicked.connect(self.show_ph_env_options)
        self.bin_pxy_toolbtn.clicked.connect(self.show_bin_pxy_options)
        self.p_rho_toolbtn.clicked.connect(self.show_p_rho_options)

        self.plot_type_btn_group.buttonClicked.connect(self.change_plot_type)

        # Setup for plot window
        self.canvas = MplCanvas(self.component_data["Names"])
        self.toolbar = NavigationToolbar2QT(self.canvas, self)
        self.toolbar.hide()
        self.canvas.hide()
        self.plot_layout.addWidget(self.toolbar)
        self.plot_layout.addWidget(self.canvas)

        self.init_isopleth_btns()
        self.plot_button.clicked.connect(self.plot)

    def get_thermopack(self):
        """
        :return Thermopack instance depending on model category
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

    def init_tp(self):
        """
        Initiates thermopack with the selected model options and interaction parameters
        """
        comp_list = self.component_data["Identities"]
        comps = ",".join(comp_list)
        model_ref = self.settings["Model options"]["Reference"]

        category = self.settings["Model category"]
        if category in ["Cubic", "CPA"]:
            eos = self.settings["EOS"]
            mixing = self.settings["Model options"]["Mixing rule"]
            alpha = self.settings["Model options"]["Alpha correlation"]

            self.tp.init(comps=comps, eos=eos, mixing=mixing, alpha=alpha, parameter_reference=model_ref)

            # TODO: Sjekk at instansieringen av interaksjonskoeffisienter faktisk fungerer
            if "Coefficient matrices" in self.component_data:
                if mixing == "vdW" and "K" in self.component_data["Coefficient matrices"].keys():
                    matrix = self.component_data["Coefficient matrices"]["K"]

                    for row in range(len(matrix)):
                        for col in range(len(matrix)):

                            c1 = comp_list[row]
                            c2 = comp_list[col]
                            index1 = self.tp.getcompindex(c1)
                            index2 = self.tp.getcompindex(c2)
                            if row != col:
                                self.tp.set_kij(index1, index2, matrix[row][col])

                elif mixing in ["HV1", "HV2"] and "Alpha" in self.component_data["Coefficient matrices"].keys():
                    alpha_matrix = self.component_data["Coefficient matrices"]["Alpha"]
                    a_matrix = self.component_data["Coefficient matrices"]["A"]
                    b_matrix = self.component_data["Coefficient matrices"]["B"]
                    c_matrix = self.component_data["Coefficient matrices"]["C"]

                    for row in range(len(a_matrix)):
                        for col in range(len(a_matrix)):

                            c1 = comp_list[row]
                            c2 = comp_list[col]
                            index1 = self.tp.getcompindex(c1)
                            index2 = self.tp.getcompindex(c2)

                            if row != col and row < col:
                                alpha_ij = alpha_matrix[row][col]
                                alpha_ji = alpha_matrix[col][row]
                                a_ij = a_matrix[row][col]
                                a_ji = a_matrix[col][row]
                                b_ij = b_matrix[row][col]
                                b_ji = b_matrix[col][row]

                                if mixing == "HV2" and "C" in self.component_data["Coefficient matrices"].keys():
                                    c_ij = c_matrix[row][col]
                                    c_ji = c_matrix[col][row]
                                else:
                                    c_ij = 0.0
                                    c_ji = 0.0

                                self.tp.set_hv_param(index1, index2, alpha_ij, alpha_ji,
                                                     a_ij, a_ji, b_ij, b_ji, c_ij, c_ji)

        elif category == "PC-SAFT":
            self.tp.init(comps=comps, parameter_reference=model_ref)

            if "K" in self.component_data["Coefficient matrices"].keys():
                matrix = self.component_data["Coefficient matrices"]["K"]

                for row in range(len(matrix)):
                    for col in range(len(matrix)):

                        c1 = comp_list[row]
                        c2 = comp_list[col]
                        index1 = self.tp.getcompindex(c1)
                        index2 = self.tp.getcompindex(c2)

                        if row != col:
                            self.tp.set_kij(index1, index2, matrix[row][col])

        elif category == "SAFT-VR Mie":
            self.tp.init(comps=comps, parameter_reference=model_ref)
            a1 = self.settings["Model options"]["A1"]
            a2 = self.settings["Model options"]["A2"]
            a3 = self.settings["Model options"]["A3"]
            hard_sphere = self.settings["Model options"]["Hard sphere"]
            chain = self.settings["Model options"]["Chain"]

            self.tp.model_control_a1(a1)
            self.tp.model_control_a2(a2)
            self.tp.model_control_a3(a3)
            self.tp.model_control_hard_sphere(hard_sphere)
            self.tp.model_control_chain(chain)

            if "epsilon" in self.component_data["Coefficient matrices"].keys():

                epsilon_matrix = self.component_data["Coefficient matrices"]["epsilon"]
                sigma_matrix = self.component_data["Coefficient matrices"]["sigma"]
                gamma_matrix = self.component_data["Coefficient matrices"]["gamma"]

                for row in range(len(epsilon_matrix)):
                    for col in range(len(epsilon_matrix)):

                        c1 = comp_list[row]
                        c2 = comp_list[col]
                        index1 = self.tp.getcompindex(c1)
                        index2 = self.tp.getcompindex(c2)
                        if row != col:
                            self.tp.set_eps_kij(index1, index2, epsilon_matrix[row][col])
                            self.tp.set_sigma_lij(index1, index2, sigma_matrix[row][col])
                            self.tp.set_lr_gammaij(index1, index2, gamma_matrix[row][col])

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
            self.molar_fractions_box.setEnabled(True)
            self.primary_vars_box.setEnabled(True)
        elif btn.text() == "Binary pxy":
            self.molar_fractions_box.setEnabled(False)
            self.primary_vars_box.setEnabled(False)
        elif btn.text() == "Pressure density":
            self.molar_fractions_box.setEnabled(True)
            self.primary_vars_box.setEnabled(False)
        elif btn.text() == "Global binary":
            self.molar_fractions_box.setEnabled(False)
            self.primary_vars_box.setEnabled(False)
        else:
            pass

    def show_ph_env_options(self):
        """
        Opens an option window where initial parameters for phase envelope plot can be set
        """
        options_window = PhaseEnvelopeOptionsWindow()
        options_window.exec_()
        options_window.button_box.accepted.connect(self.save_ph_env_options)

    def save_ph_env_options(self):
        """
        Saves selected initial parameters for phase envelope plot
        """
        pass

    def show_bin_pxy_options(self):
        """
        Opens an option window where initial parameters for binary pxy plot can be set
        """
        options_window = BinaryPXYOptionsWindow()
        options_window.exec_()
        options_window.button_box.accepted.connect(self.save_bin_pxy_options)

    def save_bin_pxy_options(self):
        """
        Saves selected initial parameters for binary pxy plot
        """
        pass

    def show_p_rho_options(self):
        """
        Opens an option window where initial parameters for pressure density plot can be set
        """
        options_window = PRhoOptionsWindow()
        options_window.exec_()
        options_window.button_box.accepted.connect(self.save_p_rho_options)

    def save_p_rho_options(self):
        """
        Saves selected initial parameters for pressure density plot
        """
        pass

    def pick_line_color(self):
        """
        Opens a color picker and sets line color for the plot
        """
        initial_color = self.line_color_preview.palette().window().color()
        color = QColorDialog.getColor(initial_color)
        style = "background-color: %s; border-style: solid; border-width: 1px; border-color: black;" % color.name()
        self.line_color_preview.setStyleSheet(style)

    def pick_point_color(self):
        """
        Opens a color picker and sets point color for the plot
        """
        initial_color = self.point_color_preview.palette().window().color()
        color = QColorDialog.getColor(initial_color)
        style = "background-color: %s; border-style: solid; border-width: 1px; border-color: black;" % color.name()
        self.point_color_preview.setStyleSheet(style)

    def change_fraction(self, value, comp_name):
        """
        Changes the mole fraction of a component

        :param float value: New mole fraction
        :param str comp_name: Name of the component
        """
        index = self.component_data["Names"].index(comp_name)
        self.component_data["Fractions"][index] = value

    def plot(self):
        """
        Checks type of plot selected, gets the correct parameters, inits thermopack,
        and calls the correct plot function in MplCanvas
        """
        category = self.settings["Model category"]
        plot_type = self.plot_type_btn_group.checkedButton().text()
        prim_vars = self.prim_vars_btn_group.checkedButton().text()

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

        self.init_tp()

        fractions = np.array(self.component_data["Fractions"])

        if self.canvas.empty:
            self.canvas.axes = self.canvas.fig.add_subplot(111)
            self.canvas.empty = False

        self.canvas.axes.cla()

        grid_on = self.grid_checkbox.isChecked()
        line_color = self.line_color_preview.palette().window().color().name()
        point_color = self.point_color_preview.palette().window().color().name()

        if plot_type in ["Phase envelope", "Pressure density"]:
            mole_fraction_sum = np.sum(fractions)

            if mole_fraction_sum != 1.00:
                msg = MolarFractionsErrorMsg(mole_fraction_sum)
                msg.exec_()
                return

        if plot_type == "Phase envelope":
            self.canvas.plot_envelope(self.tp, prim_vars, fractions, line_color, point_color, grid_on)
            self.canvas.show()
            self.toolbar.show()

        elif plot_type == "Binary pxy":
            self.canvas.plot_binary_pxy(self.tp, line_color, grid_on)
            self.canvas.show()
            self.toolbar.show()

        elif plot_type == "Pressure density":
            self.canvas.plot_pressure_density(self.tp, fractions, line_color, point_color, grid_on)
            self.canvas.show()
            self.toolbar.show()

        elif plot_type == "Global binary":
            self.canvas.plot_global_binary(self.tp, grid_on)
            self.canvas.show()
            self.toolbar.show()

        else:
            pass

        self.isopleth_btn_stack.show()


class PhaseEnvelopeOptionsWindow(QDialog):
    """
    A window containing the parameters for the phase envelope plot
    """
    def __init__(self):
        QDialog.__init__(self)
        loadUi("widgets/layouts/ph_env_options.ui", self)
        self.setWindowTitle("Phase envelope options")


class BinaryPXYOptionsWindow(QDialog):
    """
    A window containing the parameters for the binary pxy plot
    """
    def __init__(self):
        QDialog.__init__(self)
        loadUi("widgets/layouts/bin_pxy_options.ui", self)
        self.setWindowTitle("Binary pxy options")


class PRhoOptionsWindow(QDialog):
    """
    A window containing the parameters for the pressure density plot
    """
    def __init__(self):
        QDialog.__init__(self)
        loadUi("widgets/layouts/bin_pxy_options.ui", self)
        self.setWindowTitle("Pressure density options")


class GlobalBinaryOptionsWindow(QDialog):
    """
    A window containing the parameters for the global binary plot
    """
    def __init__(self):
        QDialog.__init__(self)
        loadUi("widgets/layouts/bin_pxy_options.ui", self)
        self.setWindowTitle("Global binary options")


class MolarFractionsErrorMsg(QMessageBox):
    """
    Alerts the user that the sum of the molar fractions don't add up to 1
    """
    def __init__(self, total):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("Molar fractions have to add up to 1.00. Currently the sum is %s." % total)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
