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


class CalcMode(QMainWindow):
    def __init__(self, component_data, settings, parent=None):
        super().__init__(parent=parent)

        loadUi("widgets/layouts/calc_mode.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.component_data = component_data
        self.settings = settings

        self.input_stack_indices = {
            "TP": 0,
            "PS": 1,
            "PH": 2,
            "UV": 3
        }

        self.show_input_params()
        self.init_fractions()

        self.flash_mode_selection.currentTextChanged.connect(self.show_input_params)

    def show_input_params(self, flash_mode="TP"):
        index = self.input_stack_indices[flash_mode]
        self.flash_input_stack.setCurrentIndex(index)

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

    def change_fraction(self, value, comp_name):
        index = self.component_data["Names"].index(comp_name)
        self.component_data["Fractions"][index] = value
