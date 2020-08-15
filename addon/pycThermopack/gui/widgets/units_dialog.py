from PyQt5.QtWidgets import QWidget
from PyQt5.uic import loadUi
from PyQt5.QtCore import pyqtSignal

from gui.utils import get_default_units


class UnitsDialog(QWidget):
    """
    A window where the preferred units for the application can be set
    """

    units_changed = pyqtSignal(dict)

    def __init__(self, units_data, parent=None):
        super().__init__(parent=parent)
        loadUi("gui/layouts/units.ui", self)
        self.setWindowTitle("Units")

        self.units_data = units_data

        self.default_units = get_default_units()

        self.populate_widget()

        self.temp_dropdown.currentTextChanged.connect(self.on_unit_change)
        self.press_dropdown.currentTextChanged.connect(self.on_unit_change)
        self.volume_dropdown.currentTextChanged.connect(self.on_unit_change)
        self.energy_dropdown.currentTextChanged.connect(self.on_unit_change)
        self.amount_dropdown.currentTextChanged.connect(self.on_unit_change)
        self.speed_dropdown.currentTextChanged.connect(self.on_unit_change)

        self.cancel_btn.clicked.connect(self.close)
        self.save_btn.clicked.connect(self.on_save)
        self.restore_btn.clicked.connect(self.restore_defaults)

    def populate_widget(self):
        """
        Adds available unit choices to the different dropdowns, and selects the currently chosen unit
        """
        self.temp_dropdown.addItems(self.units_data["Choices"]["Temperature"])
        self.volume_dropdown.addItems(self.units_data["Choices"]["Volume"])
        self.press_dropdown.addItems(self.units_data["Choices"]["Pressure"])
        self.energy_dropdown.addItems(self.units_data["Choices"]["Energy"])
        self.amount_dropdown.addItems(self.units_data["Choices"]["Amount"])
        self.speed_dropdown.addItems(self.units_data["Choices"]["Speed"])

        temp_index = self.temp_dropdown.findText(self.units_data["Selected"]["Temperature"])
        vol_index = self.volume_dropdown.findText(self.units_data["Selected"]["Volume"])
        press_index = self.press_dropdown.findText(self.units_data["Selected"]["Pressure"])
        energy_index = self.energy_dropdown.findText(self.units_data["Selected"]["Energy"])
        amount_index = self.amount_dropdown.findText(self.units_data["Selected"]["Amount"])
        speed_index = self.speed_dropdown.findText(self.units_data["Selected"]["Speed"])

        self.temp_dropdown.setCurrentIndex(temp_index)
        self.volume_dropdown.setCurrentIndex(vol_index)
        self.press_dropdown.setCurrentIndex(press_index)
        self.energy_dropdown.setCurrentIndex(energy_index)
        self.amount_dropdown.setCurrentIndex(amount_index)
        self.speed_dropdown.setCurrentIndex(speed_index)

    def on_unit_change(self):
        """
        When a unit has been changed, the save button is enabled
        """
        self.save_btn.setEnabled(True)

    def on_save(self):
        """
        Saves the chosen units, and closes the window
        """
        temp_unit = self.temp_dropdown.currentText()
        vol_unit = self.volume_dropdown.currentText()
        press_unit = self.press_dropdown.currentText()
        energy_unit = self.energy_dropdown.currentText()
        amount_unit = self.amount_dropdown.currentText()
        speed_unit = self.speed_dropdown.currentText()

        self.units_data["Selected"]["Temperature"] = temp_unit
        self.units_data["Selected"]["Volume"] = vol_unit
        self.units_data["Selected"]["Pressure"] = press_unit
        self.units_data["Selected"]["Energy"] = energy_unit
        self.units_data["Selected"]["Amount"] = amount_unit
        self.units_data["Selected"]["Speed"] = speed_unit

        self.units_changed.emit(self.units_data)
        self.close()

    def restore_defaults(self):
        """
        Resets the selected units to standard SI
        """
        temp_index = self.temp_dropdown.findText(self.default_units["Temperature"])
        vol_index = self.volume_dropdown.findText(self.default_units["Volume"])
        press_index = self.press_dropdown.findText(self.default_units["Pressure"])
        energy_index = self.energy_dropdown.findText(self.default_units["Energy"])
        amount_index = self.amount_dropdown.findText(self.default_units["Amount"])
        speed_index = self.speed_dropdown.findText(self.default_units["Speed"])

        self.temp_dropdown.setCurrentIndex(temp_index)
        self.volume_dropdown.setCurrentIndex(vol_index)
        self.press_dropdown.setCurrentIndex(press_index)
        self.energy_dropdown.setCurrentIndex(energy_index)
        self.amount_dropdown.setCurrentIndex(amount_index)
        self.speed_dropdown.setCurrentIndex(speed_index)
