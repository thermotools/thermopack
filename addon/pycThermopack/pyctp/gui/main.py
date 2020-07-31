from PyQt5.QtWidgets import QMainWindow, QApplication, QTreeWidgetItem, QSplashScreen, QFileDialog, \
    QActionGroup, QLabel
from PyQt5.uic import loadUi
from PyQt5.QtGui import QIcon, QPixmap
from PyQt5.QtCore import QCoreApplication, Qt, QTimer

import sys
import os

from gui.widgets.component_select_widget import ComponentSelectWidget, ComponentEditWidget
from gui.widgets.model_select_widget import ModelListMenuItem, ModelSelectWidget
from gui.widgets.go_to_plot_mode_popup import GoToPlotModeWidget, GoToCalcModeWidget
from gui.widgets.units_dialog import UnitsDialog

from gui.utils import get_json_data, save_json_data

# TODO: Ordne mer i menyen: PushButton for hovedmenyene med ikon for å lage ny


class ThermopackGUIApp(QMainWindow):
    """
    The main class for the Thermopack GUI Application
    """

    def __init__(self, parent=None):
        super().__init__(parent=parent)

        loadUi("main_layout.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.json_file = None

        self.data = {}
        self.set_initial_data()

        # Tree menu to the left
        self.tree_menu.expandAll()
        self.tree_menu.topLevelItem(0).setIcon(0, QIcon("icons/plus.png"))
        self.tree_menu.topLevelItem(1).setIcon(0, QIcon("icons/plus.png"))
        self.tree_menu.itemDoubleClicked.connect(self.menu_selection)

        # Menu actions
        self.action_save.triggered.connect(self.save)
        self.action_save_as.triggered.connect(self.save_as)
        self.action_open.triggered.connect(self.open_file)
        self.action_quit.triggered.connect(QCoreApplication.quit)
        self.action_units.triggered.connect(self.open_units_window)

        # Toolbar
        self.set_toolbar()

        self.tabs.hide()
        self.tabs.tabCloseRequested.connect(lambda index: self.close_tab(index))

    def set_initial_data(self):
        """
        Creates a dict to store data
        """
        if not self.json_file:
            self.data = {"Component lists": {},
                         "Model setups": {},
                         "Units": {
                             "Selected": {
                                 "Energy": "J",
                                 "Temperature": "K",
                                 "Pressure": "Pa",
                                 "Volume": "m^3",
                                 "Amount": "mol",
                                 "Speed": "m/s"
                             },
                             "Choices": {
                                 "Energy": ["J", "kJ", "MJ", "kcal"],
                                 "Temperature": ["K", "C", "F", "R"],
                                 "Pressure": ["Pa", "kPa", "MPa", "bar", "atm"],
                                 "Volume": ["m^3", "L", "mL"],
                                 "Amount": ["mol", "g", "kg"],
                                 "Speed": ["m/s", "mph"]
                             }
                         },
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

    def menu_selection(self):
        """
        Opens the correct widget (component selection or model setup) when a menu selection item is double clicked
        """
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
                    ModelListMenuItem(self.tree_menu.topLevelItem(1), model_select_widget.name, model_select_widget.id,
                                      model_select_widget)

            else:
                # Open a tab displaying info
                if parent.text(0) == "Select Components":
                    widget = ComponentEditWidget(self.data, name=item.text(0), parent=self)
                    widget.component_list_updated.connect(self.update_component_lists)
                    tab_text_prefix = "Component Edit- "

                elif parent.text(0) == "Select Models":
                    widget = item.widget
                    tab_text_prefix = "Settings- "

                else:
                    self.log("Widget doesn't exist")
                    return

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
        if is_new:
            QTreeWidgetItem(self.tree_menu.topLevelItem(0), [list_name])
        else:
            root = self.tree_menu.topLevelItem(0)

            for index in range(root.childCount()):
                if root.child(index).text(0) == old_name:
                    root.child(index).setText(0, list_name)

    def update_model_lists(self, list_name, data, id):
        """
        Creates/Updates a model setup menu item.
        :param list_name: New name of the model setup list
        :param is_new: True if this is a new model setup
        :param id: id of the model setup. Used to find and rename model selection tab
        """
        # Find correct tab and change its name
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
        self.dialog = GoToPlotModeWidget(self.data)
        self.dialog.setModal(True)
        self.dialog.show()

    def go_to_calc_mode(self):
        """
        Creates a popup where a component list and a model setup has to be chosen before calculation mode is initiated
        """
        self.dialog = GoToCalcModeWidget(self.data)
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

        self.data["Component lists"].update(loaded_component_data)
        self.data["Model setups"].update(loaded_model_setup_data)

        self.json_file = file_path
        self.setWindowTitle(self.windowTitle() + " - " + self.json_file)

        # Populate menu to the left
        self.log("Opened and loaded " + self.json_file + " successfully!")
        # Clear menu
        for list_name in loaded_component_data.keys():
            self.update_component_lists(list_name, is_new=True, old_name=None)

        for list_name in loaded_model_setup_data.keys():
            model_select_widget = ModelSelectWidget(self.data, name=list_name, parent=self)
            model_select_widget.settings_updated.connect(self.update_model_lists)
            ModelListMenuItem(self.tree_menu.topLevelItem(1), model_select_widget.name, model_select_widget.id,
                              model_select_widget)

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
    # win.open_file("json_structure.json")
    win.show()
    sys.exit(app.exec_())
