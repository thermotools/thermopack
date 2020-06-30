from PyQt5.QtWidgets import QDialog, QTableWidgetItem
from PyQt5.uic import loadUi


# TODO: Spesifisere hva som skal stå i cellene, verdi for verdi...


class ComponentInformationWindow(QDialog):
    def __init__(self, component, parent=None):
        super().__init__(parent=parent)
        loadUi("widgets/layouts/component_info.ui", self)
        self.setWindowTitle(component.name)

        # self.table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)

        row = self.table.rowCount()
        for key, value in component.json_data.items():
            self.table.insertRow(row)
            if isinstance(value, list):
                value = " , ".join(value)
            self.table.setItem(row, 0, QTableWidgetItem(str(key)))
            self.table.setItem(row, 1, QTableWidgetItem(str(value)))
            row += 1

        # self.table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
