from PyQt5.QtWidgets import QMainWindow, QApplication, QTreeWidgetItem, QSplashScreen, QFileDialog
from PyQt5.uic import loadUi
from PyQt5.QtGui import QIcon, QPixmap
from PyQt5.QtCore import QCoreApplication, Qt, QTimer

import sys
import os

from gui.widgets.component_select_widget import ComponentListMenuItem, ComponentSelectWidget, ComponentEditWidget
from gui.widgets.model_select_widget import ModelListMenuItem, ModelSelectWidget
from gui.widgets.go_to_plot_mode_popup import GoToPlotModeWidget
from gui.utils import get_json_data, save_json_data


# TODO:
#  Doubleclick på new comp / new model --> åpne new comp/new model-fane (maks én av hver oppe)
#  Doubleclick (eller vanlig klikk) på comp list x / model x --> Vindu hvor man kan få info om det man har tasta inn


# TODO: Lage en Load option. Skal hente frem data (JSON) fra en fil og populere
#  Denne får da to deler: Components og Settings --> To lister
#  Kan kjøre init til ComponentSelectWidget og ModelSelectWidget med kwargs


# TODO: Load fluids-filer i main. Ingen grunn til å gjøre det for hver gang ComponentSelectWidget starrtes

# TODO: Lag en Calc Mode: PT flash (+andre flasher). Velger Comp + Model, Velger T, P --> Properties
#  Kose seg med enheter og greier


class ThermopackGUIApp(QMainWindow):
    def __init__(self, parent=None, json_file=None):
        super().__init__(parent=parent)

        loadUi("main_layout.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.json_file = json_file

        self.data = {}
        self.set_initial_data()

        self.tree_menu.expandAll()
        self.tree_menu.topLevelItem(0).setIcon(0, QIcon("icons/plus.png"))
        self.tree_menu.topLevelItem(1).setIcon(0, QIcon("icons/plus.png"))
        self.tree_menu.itemDoubleClicked.connect(self.menu_selection)

        self.action_save.triggered.connect(self.save)
        self.action_save_as.triggered.connect(self.save_as)
        self.action_quit.triggered.connect(QCoreApplication.quit)

        # TODO: Gjør dette
        self.action_open.triggered.connect(self.open_file)

        self.tabs.hide()
        self.tabs.tabCloseRequested.connect(lambda index: self.close_tab(index))

        self.plot_mode_btn.setIcon(QIcon("icons/curve.png"))
        self.plot_mode_btn.clicked.connect(self.go_to_plot_mode)

        self.calc_mode_btn.setIcon(QIcon("icons/calculator.png"))
        self.calc_mode_btn.clicked.connect(self.go_to_calc_mode)

    def set_initial_data(self):
        if self.json_file:
            self.data = get_json_data(self.json_file)
        else:
            self.data = {"Component lists": {},
                         "Model setups": {}
                         }

    def menu_selection(self):
        item = self.tree_menu.currentItem()
        parent = item.parent()
        selection = item.text(0)

        if selection:
            self.tabs.show()
            index = self.tabs.currentIndex()

            if not parent:
                # If the selected item has no parent, it is a top level item

                if selection == "Select Components":
                    # If Select Components already is open, change to this tab instead of creating a new one
                    for i in range(self.tabs.count()):
                        if self.tabs.tabText(i) == "Component Selection":
                            self.tabs.setCurrentIndex(i)
                            return

                    component_select_widget = ComponentSelectWidget(self.data, parent=self)
                    index = self.tabs.addTab(component_select_widget, "Component Selection")
                    component_select_widget.component_list_updated.connect(self.update_component_lists)

                elif selection == "Select Models":
                    model_select_widget = ModelSelectWidget(self.data, parent=self)
                    index = self.tabs.addTab(model_select_widget, "Settings- " + model_select_widget.name)
                    model_select_widget.settings_updated.connect(self.update_model_lists)
                    ModelListMenuItem(self.tree_menu.topLevelItem(1), model_select_widget.name, model_select_widget.id, model_select_widget)

            else:
                # Open a tab displaying info
                if parent.text(0) == "Select Components":
                    widget = ComponentEditWidget(self.data, name=item.text(0), parent=self)
                    widget.component_list_updated.connect(self.update_component_lists)
                    tab_text_prefix = "Component Edit- "

                elif parent.text(0) == "Select Models":
                    widget = item.widget
                    tab_text_prefix = "Settings- "

                tab_is_open = False
                for i in range(self.tabs.count()):
                    if self.tabs.tabText(i)[-len(widget.name):] == widget.name:
                        index = i
                        tab_is_open = True

                if not tab_is_open:

                    index = self.tabs.addTab(widget, tab_text_prefix + widget.name)

            self.tabs.setCurrentIndex(index)

    def update_component_lists(self, list_name, is_new, old_name):
        if is_new:
            QTreeWidgetItem(self.tree_menu.topLevelItem(0), [list_name])
        else:
            root = self.tree_menu.topLevelItem(0)

            for index in range(root.childCount()):
                if root.child(index).text(0) == old_name:
                    root.child(index).setText(0, list_name)

        # Also want to update coeff widgets to include the new component list
        root = self.tree_menu.topLevelItem(1)
        for index in range(root.childCount()):
            root.child(index).widget.show_correct_coeff_widget()

    def update_model_lists(self, list_name, data, id):
        # Finn taben som hører til og endre navn på den
        self.data = data

        for index in range(self.tabs.count()):
            tab_widget = self.tabs.widget(index)

            if "Settings" in self.tabs.tabText(index):
                tab_id = tab_widget.data["Model setups"][tab_widget.name]["id"]

                if tab_id == self.data["Model setups"][list_name]["id"]:
                    self.tabs.setTabText(index, "Settings- " + list_name)

        # Find correct menu_item to rename
        root = self.tree_menu.topLevelItem(1)
        for index in range(root.childCount()):

            if root.child(index).id == id:
                root.child(index).setText(0, list_name)
                return

    def close_tab(self, index):
        self.tabs.removeTab(index)
        if self.tabs.count() < 1:
            self.tabs.hide()

    def go_to_plot_mode(self):
        self.dialog = GoToPlotModeWidget(self.data)
        self.dialog.setModal(True)
        self.dialog.show()

    def go_to_calc_mode(self):
        self.log("Go to calc mode...")

    def log(self, text):
        self.message_box.append(text)

    def open_file(self):
        self.log("Open file...")

    def save_as(self):
        file_dialog = QFileDialog()
        file_dialog.setWindowTitle('Save file')
        file_dialog.setDirectory(os.getcwd())
        file_dialog.setAcceptMode(QFileDialog.AcceptSave)
        file_dialog.setNameFilter('Text files (*.json)')
        file_dialog.setDefaultSuffix('json')

        if file_dialog.exec_() == QFileDialog.Accepted:
            file_path = file_dialog.selectedFiles()[0]

            self.json_file = file_path
            save_json_data(self.data, self.json_file)

    def save(self):
        if self.json_file:
            save_json_data(self.data, self.json_file)

        else:
            self.save_as()


if __name__ == "__main__":
    app = QApplication(sys.argv)

    splash = QSplashScreen(QPixmap("images/Thermopack logo.png"))
    splash.setWindowFlag(Qt.WindowStaysOnTopHint)
    splash.show()
    QTimer.singleShot(500, splash.close)

    win = ThermopackGUIApp()
    win.show()
    sys.exit(app.exec_())
