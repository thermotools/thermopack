from PyQt5.QtWidgets import QTableWidgetItem, QWidget
from PyQt5.uic import loadUi

from gui.utils import APP_ROOT


class ComponentInformationWindow(QWidget):
    """
    A window to show component data from the fluids-folder
    """

    def __init__(self, component, parent=None):
        super().__init__(parent=parent)
        loadUi(f"{APP_ROOT}/layouts/component_info.ui", self)
        self.setWindowTitle(component.name)

        row = self.table.rowCount()
        for key, value in component.json_data.items():
            self.table.insertRow(row)
            if isinstance(value, list):
                value = " , ".join(value)
            self.table.setItem(row, 0, QTableWidgetItem(str(key)))
            self.table.setItem(row, 1, QTableWidgetItem(str(value)))
            row += 1

        self.table.resizeColumnsToContents()
