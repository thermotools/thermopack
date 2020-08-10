from PyQt5.QtWidgets import QMainWindow, QApplication, QListWidgetItem, QSplashScreen, QFileDialog, QActionGroup, QLabel
from PyQt5.uic import loadUi
from PyQt5.QtGui import QIcon, QPixmap
from PyQt5.QtCore import QCoreApplication, Qt, QTimer

import sys
import os

from gui.widgets.component_select_widget import ComponentSelectWidget, ComponentEditWidget
from gui.widgets.model_select_widget import ModelSelectWidget
from gui.widgets.change_mode import GoToPlotModeWidget, GoToCalcModeWidget
from gui.widgets.units_dialog import UnitsDialog
from gui.widgets.about_window import AboutWindow

from gui.utils import get_json_data, save_json_data, get_default_units, MessageBox


class ThermopackGUIApp(QMainWindow):
    """
    The main class for the Thermopack GUI Application
    """

    def __init__(self, parent=None):
        super().__init__(parent=parent)

        loadUi("layouts/main_layout.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.json_file = None

        self.data = {}
        self.set_initial_data()

        self.new_composition_btn.clicked.connect(self.add_new_composition)
        self.new_model_btn.clicked.connect(self.add_new_model)

        self.compositions_list.itemDoubleClicked.connect(self.open_composition)
        self.models_list.itemDoubleClicked.connect(self.open_model)

        # Menu actions
        self.action_save.triggered.connect(self.save)
        self.action_save_as.triggered.connect(self.save_as)
        self.action_open.triggered.connect(self.open_file)
        self.action_quit.triggered.connect(QCoreApplication.quit)
        self.action_units.triggered.connect(self.open_units_window)
        self.action_about.triggered.connect(self.open_about_window)

        # Toolbar
        self.set_toolbar()

        self.tabs.hide()
        self.tabs.tabCloseRequested.connect(lambda index: self.close_tab(index))

    def set_initial_data(self):
        """
        Creates a dict to store data
        """
        if not self.json_file:
            self.data = {
                "Component lists": {},
                "Model setups": {},
                "Units": {
                    "Selected": get_default_units(),
                    "Choices": {
                        "Energy": ["J", "kJ", "MJ", "kcal"],
                        "Temperature": ["K", "C", "F", "R"],
                        "Pressure": ["Pa", "kPa", "MPa", "bar", "atm"],
                        "Volume": ["m ** 3", "L", "mL"],
                        "Amount": ["mol", "g", "kg"],
                        "Speed": ["m / s", "mph"]
                    }
                },
                "Plotting preferences": {}
            }

    def set_toolbar(self):
        """
        Creates the top toolbar
        """
        # Logo
        logo = QLabel("Thermopack")
        logo.setStyleSheet("color: #FF8B06; font: 75 28pt 'Agency FB'; padding: 5px 10px 5px 10px;")

        # Top toolbar
        toolbar = self.addToolBar("Tool bar")
        toolbar.setMovable(False)
        toolbar.actionTriggered.connect(self.handle_toolbar_action)
        toolbar.setStyleSheet("padding: 5px 10px 5px 10px;")
        toolbar.addWidget(logo)
        toolbar.addSeparator()

        action_group = QActionGroup(self)
        action_group.addAction(toolbar.addAction(QIcon("icons/open_file.png"), "Open file"))
        action_group.addAction(toolbar.addAction(QIcon("icons/save.png"), "Save"))
        toolbar.addSeparator()
        action_group.addAction(toolbar.addAction(QIcon("icons/settings.png"), "Units"))
        toolbar.addSeparator()
        action_group.addAction(toolbar.addAction(QIcon("icons/curve.png"), "Plot mode"))
        action_group.addAction(toolbar.addAction(QIcon("icons/calculator.png"), "Calculation mode"))

    def handle_toolbar_action(self, action):
        """
        Calls the correct function depending on which tool icon was clicked
        :param action: Type of tool clicked
        """
        action = action.text()
        if action == "Open file":
            self.open_file()
        elif action == "Save":
            self.save()
        elif action == "Units":
            self.open_units_window()
        elif action == "Plot mode":
            self.go_to_plot_mode()
        elif action == "Calculation mode":
            self.go_to_calc_mode()

    def open_units_window(self):
        """
        Opens a dialog where the user can change default units for the application
        """
        units_data = self.data["Units"]
        self.dialog = UnitsDialog(units_data)
        self.dialog.show()

    def open_about_window(self):
        """
        Opens a window where information about the Thermopack application is displayed
        """
        self.about_window = AboutWindow()
        self.about_window.show()

    def add_new_composition(self):
        self.tabs.show()

        # If Select Components already is open, change to this tab instead of creating a new one
        for i in range(self.tabs.count()):
            if self.tabs.tabText(i) == "Component Selection":
                self.tabs.setCurrentIndex(i)
                return

        component_select_widget = ComponentSelectWidget(self.data, parent=self)
        index = self.tabs.addTab(component_select_widget, "Component Selection")
        component_select_widget.component_list_updated.connect(self.update_component_lists)

        self.tabs.setCurrentIndex(index)

    def add_new_model(self):
        self.tabs.show()

        model_select_widget = ModelSelectWidget(self.data, parent=self)
        index = self.tabs.addTab(model_select_widget, "Model Setup - " + model_select_widget.name)
        QListWidgetItem(model_select_widget.name, parent=self.models_list)
        model_select_widget.model_name_changed.connect(self.update_model_lists)
        model_select_widget.model_setup_deleted.connect(self.delete_model_setup)

        self.tabs.setCurrentIndex(index)

    def open_composition(self, item):
        """
        Opens the correct component select widget when a menu selection item is double clicked
        """
        self.tabs.show()
        widget = ComponentEditWidget(self.data, name=item.text(), parent=self)
        widget.component_list_updated.connect(self.update_component_lists)
        widget.component_list_deleted.connect(self.delete_component_list)
        tab_text_prefix = "Edit Composition - "
        index = self.tabs.currentIndex()

        tab_is_open = False
        for i in range(self.tabs.count()):
            if self.tabs.tabText(i)[-len(widget.name):] == widget.name:
                index = i
                tab_is_open = True

        if not tab_is_open:
            index = self.tabs.addTab(widget, tab_text_prefix + widget.name)

        self.tabs.setCurrentIndex(index)

    def open_model(self, item):
        """
        Opens the correct model setup widget when a menu selection item is double clicked
        """
        self.tabs.show()
        widget = ModelSelectWidget(self.data, name=item.text(), parent=self)
        widget.model_name_changed.connect(self.update_model_lists)
        widget.model_setup_deleted.connect(self.delete_model_setup)
        tab_text_prefix = "Model Setup - "
        index = self.tabs.currentIndex()

        tab_is_open = False
        for i in range(self.tabs.count()):
            if self.tabs.tabText(i)[-len(widget.name):] == widget.name:
                index = i
                tab_is_open = True

        if not tab_is_open:
            index = self.tabs.addTab(widget, tab_text_prefix + widget.name)

        self.tabs.setCurrentIndex(index)

    def update_component_lists(self, list_name, is_new, old_name):
        """
        Creates/Updates a component list menu item. If the component list already exists, the menu item is renamed
        :param list_name: New name of the component list
        :param is_new: True if this is a new component list
        :param old_name: Previous name of the component list, to be updated
        """
        # Find correct tab and change its name
        for index in range(self.tabs.count()):
            if "Edit Composition" in self.tabs.tabText(index) and old_name in self.tabs.tabText(index):
                self.tabs.setTabText(index, "Edit Composition - " + list_name)

        if is_new:
            self.compositions_list.addItem(list_name)

        else:
            for i in range(self.compositions_list.count()):
                if self.compositions_list.item(i).text() == old_name:
                    self.compositions_list.item(i).setText(list_name)

    def delete_component_list(self, name):
        """
        Deletes a composition
        :param name: Name of composition to be deleted
        """
        # Deleting entry in the data dictionary
        try:
            del self.data["Component lists"][name]
        except KeyError:
            msg = MessageBox("Error", "Could not delete composition " + name)
            msg.exec_()
            return

        # Closing the composition tab
        for i in range(self.tabs.count()):
            if self.tabs.tabText(i)[-len(name):] == name:
                self.tabs.removeTab(i)
                break

        # Removing name from the menu
        for i in range(self.compositions_list.count()):
            if self.compositions_list.item(i).text() == name:
                self.compositions_list.takeItem(i)
                break

    def delete_model_setup(self, name):
        """
        Deletes a model setup
        :param name: Name of model setup to be deleted
        """
        # Deleting entry in the data dictionary
        try:
            del self.data["Model setups"][name]
        except KeyError:
            msg = MessageBox("Error", "Could not delete model setup " + name)
            msg.exec_()
            return

        # Closing the composition tab
        for i in range(self.tabs.count()):
            if self.tabs.tabText(i)[-len(name):] == name:
                self.tabs.removeTab(i)
                break

        # Removing name from the menu
        for i in range(self.models_list.count()):
            if self.models_list.item(i).text() == name:
                self.models_list.takeItem(i)
                break

    def update_model_lists(self, list_name, is_new, old_name):
        """
        Creates/Updates a model setup menu item. If the model setup already exists, the menu item is renamed
        :param list_name: New name of the model setup
        :param is_new: True if this is a model setup
        :param old_name: Previous name of the model setup, to be updated
        """
        # Find correct tab and change its name
        for index in range(self.tabs.count()):
            if "Model Setup" in self.tabs.tabText(index) and old_name in self.tabs.tabText(index):
                self.tabs.setTabText(index, "Model Setup - " + list_name)

        if is_new:
            self.models_list.addItem(list_name)

        else:
            for i in range(self.models_list.count()):
                if self.models_list.item(i).text() == old_name:
                    self.models_list.item(i).setText(list_name)

    def close_tab(self, index):
        """
        Closes (hides) a tab
        :param index: Index of tab to be closed
        """
        self.tabs.removeTab(index)
        if self.tabs.count() < 1:
            self.tabs.hide()

    def go_to_plot_mode(self):
        """
        Creates a popup where a component list and a model setup has to be chosen before plot mode is initiated
        """
        self.dialog = GoToPlotModeWidget(self.data, self.json_file)
        self.dialog.setModal(True)
        self.dialog.show()

    def go_to_calc_mode(self):
        """
        Creates a popup where a component list and a model setup has to be chosen before calculation mode is initiated
        """
        self.dialog = GoToCalcModeWidget(self.data, self.json_file)
        self.dialog.setModal(True)
        self.dialog.show()

    def log(self, text):
        """
        Prints text to the user in the message box
        :param text: Text to be printed
        """
        self.message_box.append(text)

    def open_file(self, file_path=None):
        """
        Opens and loads an existing JSON-file and populates the main window.
        :param file_path: Path to JSON-file
        """
        if not file_path:
            file_dialog = QFileDialog()
            file_dialog.setWindowTitle("Open File")
            file_dialog.setDirectory(os.getcwd())
            file_dialog.setNameFilter('Text files (*.json)')

            if file_dialog.exec_() == QFileDialog.Accepted:
                file_path = file_dialog.selectedFiles()[0]
            else:
                return

        loaded_data = get_json_data(file_path)
        loaded_component_data = loaded_data["Component lists"]
        loaded_model_setup_data = loaded_data["Model setups"]
        loaded_units_data = loaded_data["Units"]
        loaded_plotting_preferences = loaded_data["Plotting preferences"]

        # If loaded data contains names which currently exist, an error message will be displayed
        loaded_data_is_compatible = True
        for i in range(self.compositions_list.count()):
            if self.compositions_list.item(i).text() in loaded_component_data.keys():
                loaded_data_is_compatible = False
                name = self.compositions_list.item(i).text()

        for i in range(self.models_list.count()):
            if self.models_list.item(i).text() in loaded_model_setup_data.keys():
                loaded_data_is_compatible = False
                name = self.models_list.item(i).text()

        if not loaded_data_is_compatible:
            error_msg = "Could not load data from %s. %s already exists. " \
                        "Either change its name or delete it before trying to load again." \
                        % (file_path, name)
            msg = MessageBox("Error", error_msg)
            msg.exec_()
            return

        self.data["Component lists"].update(loaded_component_data)
        self.data["Model setups"].update(loaded_model_setup_data)
        self.data["Units"].update(loaded_units_data)
        self.data["Plotting preferences"].update(loaded_plotting_preferences)

        self.json_file = file_path
        self.setWindowTitle(self.windowTitle() + " - " + self.json_file)

        # Populate menus to the left
        self.log("Opened and loaded " + self.json_file + " successfully!")
        # Clear menu
        for list_name in loaded_component_data.keys():
            self.update_component_lists(list_name, is_new=True, old_name=None)

        for list_name in loaded_model_setup_data.keys():
            model_select_widget = ModelSelectWidget(self.data, name=list_name, parent=self)
            model_select_widget.model_setup_deleted.connect(self.delete_model_setup)
            model_select_widget.model_name_changed.connect(self.update_model_lists)
            QListWidgetItem(list_name, parent=self.models_list)

        self.save()

    def save_as(self):
        """
        Opens a file dialog to choose a place to save a JSON-file containing the information from this session
        """
        file_dialog = QFileDialog()
        file_dialog.setWindowTitle('Save File')
        file_dialog.setDirectory(os.getcwd())
        file_dialog.setAcceptMode(QFileDialog.AcceptSave)
        file_dialog.setNameFilter('Text files (*.json)')
        file_dialog.setDefaultSuffix('json')

        if file_dialog.exec_() == QFileDialog.Accepted:
            file_path = file_dialog.selectedFiles()[0]

            self.json_file = file_path
            self.setWindowTitle(self.windowTitle() + " - " + self.json_file)
            save_json_data(self.data, self.json_file)
            self.log("Saved file.")

    def save(self):
        """
        Saves data to the current JSON-file. If none is selected, a file dialog is opened.
        """
        if self.json_file:
            save_json_data(self.data, self.json_file)
            self.log("Saved file.")

        else:
            self.save_as()


if __name__ == "__main__":
    app = QApplication(sys.argv)

    # Load stylesheet
    stylesheet = open("stylesheet.qss", "r")
    app.setStyleSheet(stylesheet.read())
    stylesheet.close()

    # Image shown when application is loading
    splash = QSplashScreen(QPixmap("images/Thermopack logo.png"))
    splash.setWindowFlag(Qt.WindowStaysOnTopHint)
    splash.show()
    QTimer.singleShot(500, splash.close)

    win = ThermopackGUIApp()
    win.show()
    sys.exit(app.exec_())
