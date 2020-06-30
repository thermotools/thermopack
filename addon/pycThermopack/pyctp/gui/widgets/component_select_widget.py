from PyQt5.QtWidgets import QWidget, QTableWidgetItem, QMessageBox, QHeaderView
from PyQt5.uic import loadUi
from PyQt5.QtCore import pyqtSignal

import os
import json

from gui.classes.component import Component
from gui.widgets.component_info import ComponentInformationWindow
from gui.utils import get_unique_name


# TODO: Vise lagrede komposisjoner i en liste av noe slag
# TODO: Søkefunksjon. QSortFilterProxyModel(). Bookmarked in Chrome
# TODO: Mulighet for å slette en lagret komposisjon


class ComponentSelectWidget(QWidget):
    def __init__(self, data, parent=None):
        QWidget.__init__(self, parent)

        loadUi("widgets/layouts/component_select_widget.ui", self)

        self.data = data

        # Components available in the 'fluids' folder
        self.available_components = {}

        # Chosen final composition (can store multiple)
        self.final_compositions = {}

        # To keep track of currently selected components
        self.selected_components = []

        self.set_component_list_placeholder_name()

        # Buttons for adding or removing components from selected component list
        self.button_add.clicked.connect(self.choose_components)
        self.button_remove.clicked.connect(self.remove_components)

        # Displays a new window with more information about the chosen component
        self.component_choices_table.itemDoubleClicked.connect(self.display_component_info)

        self.set_name_edit.returnPressed.connect(self.set_name_edit.clearFocus)

        # Confirm selected component list
        self.choose_components_button.clicked.connect(self.save_composition)

        self.search_box.textChanged.connect(self.search)

        # Get available fluids and populate table
        for root, dirs, files in os.walk("../../../../fluids"):
            for file_name in files:
                self.populate_component_choices(os.path.join(root, file_name))

    component_list_updated = pyqtSignal(str, dict)

    def set_component_list_placeholder_name(self):
        self.set_name_edit.clear()
        name = get_unique_name("Composition", self.final_compositions.keys())
        self.set_name_edit.setPlaceholderText(name)

    def populate_component_choices(self, file_name):
        file = open(file_name, "r")

        component_data = json.load(file)
        name = component_data["name"]

        component = Component(name, component_data)
        self.available_components[name] = component

        self.insert_component(self.component_choices_table, component)

        file.close()

    def choose_components(self):
        selected_rows = self.component_choices_table.selectionModel().selectedRows()
        if selected_rows:
            for row_object in selected_rows:

                name = self.component_choices_table.item(row_object.row(), 0).text()
                component = self.available_components[name]

                if component.name not in self.selected_components:
                    self.selected_components.append(component.name)
                    self.insert_component(self.selected_components_table, component)

    def insert_component(self, table, component):
        row = table.rowCount()
        table.insertRow(row)

        table.setItem(row, 0, QTableWidgetItem(component.name))
        table.setItem(row, 1, QTableWidgetItem(component.json_data["formula"]))
        table.setItem(row, 2, QTableWidgetItem(component.json_data["cas_number"]))

    def remove_components(self):
        selected_rows = self.selected_components_table.selectionModel().selectedRows()
        rows_to_be_deleted = (row_object.row() for row_object in selected_rows)

        for row_object in selected_rows:
            name = self.selected_components_table.item(row_object.row(), 0).text()
            self.selected_components.pop(self.selected_components.index(name))

        for row in sorted(rows_to_be_deleted, reverse=True):
            self.selected_components_table.removeRow(row)

    def display_component_info(self, item):
        name = self.component_choices_table.item(item.row(), 0).text()
        component = self.available_components[name]
        dialog = ComponentInformationWindow(component, self)
        dialog.show()

    def save_composition(self):
        if self.selected_components:

            if self.selected_components not in self.final_compositions.values():
                if self.set_name_edit.text():
                    list_name = self.set_name_edit.text()
                else:
                    list_name = self.set_name_edit.placeholderText()

                if list_name not in self.data["Component lists"]:

                    component_list = self.selected_components.copy()
                    self.final_compositions[list_name] = component_list

                    id_list = [self.available_components[name].json_data["ident"] for name in component_list]
                    self.data["Component lists"][list_name] = {"Names": component_list,
                                                               "Identities": id_list}

                    self.component_list_updated.emit(list_name, self.data)

                    # Clear table and selection
                    self.selected_components_table.setRowCount(0)
                    self.selected_components = []

                    self.set_component_list_placeholder_name()
                else:
                    msg = ListNameAlreadyExistsMessageBox(list_name)

            else:
                msg = CompositionAlreadyExistsMessageBox(self.selected_components, self.final_compositions)

        else:
            msg = NoComponentsChosenMessageBox()

    def search(self, search_text):
        pass


class NoComponentsChosenMessageBox(QMessageBox):
    def __init__(self):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("No components are chosen")
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)


class CompositionAlreadyExistsMessageBox(QMessageBox):
    def __init__(self, selected_components, final_compositions):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("This composition is already stored as " + str(list(final_compositions.keys())[list(
            final_compositions.values()).index(selected_components)]))
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
        self.exec_()


class ListNameAlreadyExistsMessageBox(QMessageBox):
    def __init__(self, list_name):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("A component list with this name (%s) already exists" % list_name)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
