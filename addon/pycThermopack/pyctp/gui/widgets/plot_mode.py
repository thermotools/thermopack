from PyQt5.QtWidgets import QMainWindow, QRadioButton, QButtonGroup, QDoubleSpinBox, QColorDialog, QMessageBox, QDialog
from PyQt5.uic import loadUi

from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT

from gui.widgets.mpl_canvas import MplCanvas

import numpy as np

from thermo import thermopack
from cubic import cubic
from cpa import cpa
from pcsaft import pcsaft
from saftvrmie import saftvrmie


# TODO: Mulighet for å plotte flere ting oppå hverandre (Default: Clear hver gang, mulig å endre eks checkbox)

# TODO: Når isopleter toggles on/off, fjern/legg til i legend

# TODO: Lagre paramtere i self.plot_settings = {}


class PlotMode(QMainWindow):
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
        category = self.settings["Model category"]
        if category == "Cubic":
            return cubic()
        elif category == "CPA":
            return cpa()
        else:
            return None

    def init_plot_modes(self):
        if len(self.component_data["Names"]) != 2:
            self.binary_pxy_btn.setEnabled(False)

    def init_model_options(self):
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

    def init_fractions(self):
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
        category = self.settings["Model category"]
        if category in ["Cubic", "CPA"]:
            comps = ",".join(self.component_data["Identities"])
            eos = self.settings["EOS"]
            mixing = self.settings["Model options"]["Mixing rule"]
            alpha = self.settings["Model options"]["Alpha correlation"]
            model_ref = self.settings["Model options"]["Reference"]

            self.tp.init(comps=comps, eos=eos, mixing=mixing, alpha=alpha, parameter_reference=model_ref)

    def init_isopleth_btns(self):
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

        self.btn_stack.hide()

    def change_plot_type(self, btn):
        if btn.text() == "Phase envelope":
            self.molar_fractions_box.setEnabled(True)
            self.primary_vars_box.setEnabled(True)
        elif btn.text() == "Binary pxy":
            self.molar_fractions_box.setEnabled(False)
            self.primary_vars_box.setEnabled(False)
        else:
            pass

    def show_ph_env_options(self):
        options_window = PhaseEnvelopeOptionsWindow()
        options_window.exec_()
        options_window.button_box.accepted.connect(self.save_ph_env_options)

    def save_ph_env_options(self):
        pass

    def show_bin_pxy_options(self):
        options_window = BinaryPXYOptionsWindow()
        options_window.exec_()
        options_window.button_box.accepted.connect(self.save_bin_pxy_options)

    def save_bin_pxy_options(self):
        pass

    def pick_line_color(self):
        initial_color = self.line_color_preview.palette().window().color()
        color = QColorDialog.getColor(initial_color)
        style = "background-color: %s; border-style: solid; border-width: 1px; border-color: black;" % color.name()
        self.line_color_preview.setStyleSheet(style)

    def pick_point_color(self):
        initial_color = self.point_color_preview.palette().window().color()
        color = QColorDialog.getColor(initial_color)
        style = "background-color: %s; border-style: solid; border-width: 1px; border-color: black;" % color.name()
        self.point_color_preview.setStyleSheet(style)

    def change_fraction(self, value, comp_name):
        index = self.component_data["Names"].index(comp_name)
        self.component_data["Fractions"][index] = value

    def plot(self):
        plot_type = self.plot_type_btn_group.checkedButton().text()
        prim_vars = self.prim_vars_btn_group.checkedButton().text()
        eos = self.model_btn_group.checkedButton().text()
        fractions = np.array(self.component_data["Fractions"])

        if self.settings["EOS"] != eos:
            self.settings["EOS"] = eos
            self.init_tp()

        if self.canvas.empty:
            self.canvas.axes = self.canvas.fig.add_subplot(111)
            self.canvas.empty = False

        self.canvas.axes.cla()

        grid_on = self.grid_checkbox.isChecked()
        line_color = self.line_color_preview.palette().window().color().name()
        point_color = self.point_color_preview.palette().window().color().name()

        if plot_type == "Phase envelope":
            mole_fraction_sum = np.sum(fractions)

            if mole_fraction_sum != 1.00:
                msg = MolarFractionsErrorMsg(mole_fraction_sum)
                msg.exec_()
                return
            else:
                self.canvas.plot_envelope(self.tp, prim_vars, fractions, line_color, point_color, grid_on)
                self.canvas.show()
                self.toolbar.show()

        elif plot_type == "Binary pxy":
            self.canvas.plot_binary_pxy(self.tp, line_color, grid_on)
            self.canvas.show()
            self.toolbar.show()

        else:
            pass

        self.btn_stack.show()


class PhaseEnvelopeOptionsWindow(QDialog):
    def __init__(self):
        QDialog.__init__(self)
        loadUi("widgets/layouts/ph_env_options.ui", self)
        self.setWindowTitle("Phase envelope options")


class BinaryPXYOptionsWindow(QDialog):
    def __init__(self):
        QDialog.__init__(self)
        loadUi("widgets/layouts/bin_pxy_options.ui", self)
        self.setWindowTitle("Binary pxy options")


class MolarFractionsErrorMsg(QMessageBox):
    def __init__(self, total):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("Molar fractions have to add up to 1,00. Currently the sum is %s." % total)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
