from PyQt5.QtWidgets import QMainWindow, QApplication, QTreeWidgetItem, QSplashScreen, QFileDialog, \
    QActionGroup, QLabel, QDialog
from PyQt5.uic import loadUi
from PyQt5.QtGui import QIcon, QPixmap
from PyQt5.QtCore import QCoreApplication, Qt, QTimer

import sys
import os

from gui.widgets.component_select_widget import ComponentSelectWidget, ComponentEditWidget
from gui.widgets.model_select_widget import ModelListMenuItem, ModelSelectWidget
from gui.widgets.go_to_plot_mode_popup import GoToPlotModeWidget, GoToCalcModeWidget
from gui.utils import get_json_data, save_json_data

# TODO: Funksjonalitet for enheter:


class ThermopackGUIApp(QMainWindow):
    def __init__(self, parent=None, json_file=None):
        super().__init__(parent=parent)

        loadUi("main_layout.ui", self)
        self.setWindowTitle("Thermopack")
        self.showMaximized()

        self.json_file = json_file

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
        self.action_units.triggered.connect(self.open_settings)

        # Toolbar
        self.set_toolbar()

        self.tabs.hide()
        self.tabs.tabCloseRequested.connect(lambda index: self.close_tab(index))

    def set_initial_data(self):
        if self.json_file:
            self.data = get_json_data(self.json_file)
        else:
            self.data = {"Component lists": {},
                         "Model setups": {}
                         }

    def set_toolbar(self):
        # Logo
        logo = QLabel("Thermopack")
        logo.setStyleSheet("color: #FF8B06; font: 75 28pt 'Agency FB'; padding: 5px 10px 5px 10px;")

        # Top toolbar
        toolbar = self.addToolBar("Tekst")
        toolbar.setMovable(False)
        toolbar.actionTriggered.connect(self.handle_toolbar_action)
        toolbar.setStyleSheet("padding: 5px 10px 5px 10px;")
        toolbar.addWidget(logo)
        toolbar.addSeparator()

        action_group = QActionGroup(self)
        action_group.addAction(toolbar.addAction(QIcon("icons/open_file.png"), "Open file"))
        action_group.addAction(toolbar.addAction(QIcon("icons/save.png"), "Save"))
        toolbar.addSeparator()
        action_group.addAction(toolbar.addAction(QIcon("icons/settings.png"), "Settings and preferences"))
        toolbar.addSeparator()
        action_group.addAction(toolbar.addAction(QIcon("icons/curve.png"), "Plot mode"))
        action_group.addAction(toolbar.addAction(QIcon("icons/calculator.png"), "Calculation mode"))

    def handle_toolbar_action(self, action):
        action = action.text()
        if action == "Open file":
            self.open_file()
        elif action == "Save":
            self.save()
        elif action == "Settings and preferences":
            self.open_settings()
        elif action == "Plot mode":
            self.go_to_plot_mode()
        elif action == "Calculation mode":
            self.go_to_calc_mode()

    def open_settings(self):
        dialog = QDialog()
        dialog.exec_()

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
        if is_new:
            QTreeWidgetItem(self.tree_menu.topLevelItem(0), [list_name])
        else:
            root = self.tree_menu.topLevelItem(0)

            for index in range(root.childCount()):
                if root.child(index).text(0) == old_name:
                    root.child(index).setText(0, list_name)

    def update_model_lists(self, list_name, data, id):
        # TODO: Noe er fishy her. Klikker når jeg har flere enn to modeller
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
        self.tabs.removeTab(index)
        if self.tabs.count() < 1:
            self.tabs.hide()

    def go_to_plot_mode(self):
        self.dialog = GoToPlotModeWidget(self.data)
        self.dialog.setModal(True)
        self.dialog.show()

    def go_to_calc_mode(self):
        self.dialog = GoToCalcModeWidget(self.data)
        self.dialog.setModal(True)
        self.dialog.show()

    def log(self, text):
        self.message_box.append(text)

    def open_file(self, file_path=None):
        if not file_path:
            file_dialog = QFileDialog()
            file_dialog.setWindowTitle("Open File")
            file_dialog.setDirectory(os.getcwd())
            file_dialog.setNameFilter('Text files (*.json)')

            if file_dialog.exec_() == QFileDialog.Accepted:
                file_path = file_dialog.selectedFiles()[0]

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
        if self.json_file:
            save_json_data(self.data, self.json_file)
            self.log("Saved file.")

        else:
            self.save_as()


if __name__ == "__main__":
    app = QApplication(sys.argv)

    splash = QSplashScreen(QPixmap("images/Thermopack logo.png"))
    splash.setWindowFlag(Qt.WindowStaysOnTopHint)
    splash.show()
    QTimer.singleShot(500, splash.close)

    win = ThermopackGUIApp()
    win.open_file("test.json")
    win.show()
    sys.exit(app.exec_())
