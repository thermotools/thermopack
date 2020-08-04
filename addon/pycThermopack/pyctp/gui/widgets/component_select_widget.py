from PyQt5.QtWidgets import QWidget, QTableWidgetItem, QMessageBox
from PyQt5.uic import loadUi
from PyQt5.QtCore import pyqtSignal

from gui.widgets.component_info import ComponentInformationWindow
from gui.utils import get_unique_name, get_fluids


# TODO: Mulighet for å slette en lagret komposisjon
# TODO: Resize table to fit contents


class ComponentSelectWidget(QWidget):
    """
    Widget for choosing a fluid composition. Contains a table with the available components, Add and Remove buttons,
    and a field for entering a name for the composition. When double-clicking on a component, a window displaying all
    the fluid data opens.
    """
    def __init__(self, data, parent=None):
        QWidget.__init__(self, parent)

        loadUi("widgets/layouts/component_select_widget.ui", self)

        self.data = data

        # Components available in the 'fluids' folder
        self.fluids = get_fluids()
        # Chosen final composition (list_name: component_list)
        self.final_compositions = {}
        # To keep track of currently selected components
        self.selected_components = []

        self.set_component_list_placeholder_name()

        self.populate_component_choices()

        # Buttons for adding or removing components from selected component list
        self.button_add.clicked.connect(self.choose_components)
        self.button_remove.clicked.connect(self.remove_components)

        # Displays a new window with more information about the chosen component
        self.component_choices_table.itemDoubleClicked.connect(self.display_component_info)

        self.set_name_edit.returnPressed.connect(self.set_name_edit.clearFocus)

        # Confirm selected component list
        self.choose_components_button.clicked.connect(self.save_composition)

        self.search_box.textChanged.connect(self.search)

    component_list_updated = pyqtSignal(str, bool, str)

    def set_component_list_placeholder_name(self):
        """
        Sets a unique placeholder name for the composition
        """
        self.set_name_edit.clear()
        name = get_unique_name("Composition", self.final_compositions.keys())
        self.set_name_edit.setPlaceholderText(name)

    def populate_component_choices(self):
        """
        Inserts all available components into the components table
        """
        for component in self.fluids.values():
            self.insert_component(self.component_choices_table, component)

    def choose_components(self):
        """
        Finds the selected components from the available components table, and inserts them to the table containing
        the currently chosen components
        """
        selected_rows = self.component_choices_table.selectionModel().selectedRows()
        if selected_rows:
            for row_object in selected_rows:

                name = self.component_choices_table.item(row_object.row(), 0).text()
                component = self.fluids[name]

                if component.name not in self.selected_components:
                    self.selected_components.append(component.name)
                    self.insert_component(self.selected_components_table, component)

    def insert_component(self, table, component):
        """
        Inserts a component to a table
        :param table: QTableWidget into which the component should be added
        :param component: Component (instance of Component class) to be inserted to the table
        """
        row = table.rowCount()
        table.insertRow(row)

        table.setItem(row, 0, QTableWidgetItem(component.name))
        table.setItem(row, 1, QTableWidgetItem(component.json_data["formula"]))
        table.setItem(row, 2, QTableWidgetItem(component.json_data["cas_number"]))

    def remove_components(self):
        """
        Removes the selected components from the current composition
        """
        selected_rows = self.selected_components_table.selectionModel().selectedRows()
        rows_to_be_deleted = (row_object.row() for row_object in selected_rows)

        for row_object in selected_rows:
            name = self.selected_components_table.item(row_object.row(), 0).text()
            self.selected_components.pop(self.selected_components.index(name))

        for row in sorted(rows_to_be_deleted, reverse=True):
            self.selected_components_table.removeRow(row)

    def display_component_info(self, item):
        """
        Opens a window containing information about the fluid which is double clicked
        :param item: Table component item
        """
        name = self.component_choices_table.item(item.row(), 0).text()
        component = self.fluids[name]
        self.dialog = ComponentInformationWindow(component)
        self.dialog.show()

    def save_composition(self):
        """
        Saves the selected composition to the session data, and emits a signal to indicate that a composition list is
        created or edited
        """
        if self.selected_components:

            if self.selected_components not in self.final_compositions.values():
                if self.set_name_edit.text():
                    list_name = self.set_name_edit.text()
                else:
                    list_name = self.set_name_edit.placeholderText()

                if list_name not in self.data["Component lists"]:

                    component_list = self.selected_components.copy()
                    self.final_compositions[list_name] = component_list

                    id_list = [self.fluids[name].json_data["ident"] for name in component_list]
                    self.data["Component lists"][list_name] = {"Names": component_list,
                                                               "Identities": id_list}

                    self.component_list_updated.emit(list_name, True, list_name)

                    # Clear table and selection
                    self.selected_components_table.setRowCount(0)
                    self.selected_components = []

                    self.set_component_list_placeholder_name()
                else:
                    msg = ListNameAlreadyExistsMessageBox(list_name)
                    msg.exec_()
                    self.set_name_edit.undo()

            else:
                msg = CompositionAlreadyExistsMessageBox(self.selected_components, self.final_compositions)
                msg.exec_()

        else:
            msg = NoComponentsChosenMessageBox()
            msg.exec_()

    def search(self, filter_text):
        """
        Search function for the available components. Hides component rows of which the inserted text does not match
        either name, aliases, identity, chemical formula or CAS number
        :param filter_text: Search text
        """
        table = self.component_choices_table
        for i in range(table.rowCount()):
            match = False

            component = self.fluids[table.item(i, 0).text()]
            aliases = component.json_data["aliases"]
            identity = component.json_data["ident"]

            if filter_text.lower() in identity.lower():
                match = True

            for alias in aliases:
                if filter_text.lower() in alias.lower():
                    match = True
                    break

            for j in range(table.columnCount()):
                item = table.item(i, j)
                if filter_text.lower() in item.text().lower():
                    match = True
                    break

            table.setRowHidden(i, not match)


class ComponentEditWidget(ComponentSelectWidget):
    """
    Widget for editing a previously saved composition (looks the same as the ComponentSelectWidget)
    """
    def __init__(self, data, name, parent=None):
        ComponentSelectWidget.__init__(self, data, parent)

        self.name = name

        self.label_title.setText("Component List Edit Menu")

        self.populate_widget()

        self.set_name_edit.editingFinished.connect(self.save_composition)

    def populate_widget(self):
        """
        Sets up the widget for the given composition. Inserts components to the selected components table
        """
        self.set_name_edit.setText(self.name)
        comp_list = [self.fluids[name] for name in self.data["Component lists"][self.name]["Names"]]
        for comp in comp_list:
            self.selected_components.append(comp.name)
            self.insert_component(self.selected_components_table, comp)

    def save_composition(self):
        """
        Saves (Updates) the composition
        """

        if self.selected_components:

            if self.set_name_edit.text():
                list_name = self.set_name_edit.text()
            elif self.set_name_edit.placeHolderText():
                list_name = self.set_name_edit.placeholderText()
            else:
                list_name = get_unique_name("Composition", self.final_compositions.keys())
                self.set_name_edit.setPlaceholderText(list_name)

            if list_name == self.name:
                component_list = self.selected_components.copy()
                self.final_compositions[list_name] = component_list

                id_list = [self.fluids[comp].json_data["ident"] for comp in component_list]
                self.data["Component lists"][list_name] = {"Names": component_list,
                                                           "Identities": id_list}

                self.component_list_updated.emit(list_name, False, self.name)
                self.name = list_name

            elif list_name not in self.data["Component lists"]:
                component_list = self.selected_components.copy()
                self.final_compositions[list_name] = component_list

                id_list = [self.fluids[comp].json_data["ident"] for comp in component_list]

                self.data["Component lists"][self.name] = {"Names": component_list,
                                                           "Identities": id_list}

                self.data["Component lists"][list_name] = self.data["Component lists"].pop(self.name)

                self.component_list_updated.emit(list_name, False, self.name)
                self.name = list_name

            else:
                msg = ListNameAlreadyExistsMessageBox(list_name)
                self.set_name_edit.undo()
                msg.exec_()

        else:
            msg = NoComponentsChosenMessageBox()
            msg.exec_()


class NoComponentsChosenMessageBox(QMessageBox):
    """
    Showed when the user tries to save a composition with no components
    """
    def __init__(self):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("No components are chosen")
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)


class CompositionAlreadyExistsMessageBox(QMessageBox):
    """
    Showed when the user tries to save a composition which already exists
    """
    def __init__(self, selected_components, final_compositions):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("This composition is already stored as " + str(list(final_compositions.keys())[list(
            final_compositions.values()).index(selected_components)]))
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)


class ListNameAlreadyExistsMessageBox(QMessageBox):
    """
    Showed when the user tries to save a composition with the name of another composition
    """
    def __init__(self, list_name):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("A component list with this name (%s) already exists" % list_name)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
