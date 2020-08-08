from PyQt5.QtWidgets import QDialog, QListWidgetItem
from PyQt5.uic import loadUi

from gui.widgets.plot_mode import PlotMode
from gui.widgets.calc_mode import CalcMode


# TODO: Når du dobbelttrykker på en complist eller setting (ev. hover) --> Vise vindu med info (ev. et (?)-ikon)


class ChangeModePopup(QDialog):
    """
    Base class for popups 'Go to Plot Mode' and 'Go to Calc Mode'
    The user selects one component list, and one model setup before proceeding
    """
    def __init__(self, data, json_file, parent=None):
        super().__init__(parent=parent)
        loadUi("layouts/go_to_plot_mode_popup.ui", self)
        self.setWindowTitle("Thermopack")

        self.json_file = json_file
        self.data = data

        self.init_composition_list()
        self.init_options_list()

        if self.composition_list.currentItem() and self.model_options_list.currentItem():
            self.go_btn.setEnabled(True)
            self.go_btn.setAutoDefault(True)
        else:
            self.close_button.setAutoDefault(True)

        self.composition_list.itemSelectionChanged.connect(self.selection_changed)
        self.model_options_list.itemSelectionChanged.connect(self.selection_changed)

        self.close_button.clicked.connect(lambda: self.close())

    def init_composition_list(self):
        """
        Displays all composition lists
        """
        component_lists = self.data["Component lists"].keys()
        for composition_name in component_lists:
            self.composition_list.addItem(QListWidgetItem(composition_name))
        if len(component_lists) > 0:
            self.composition_list.setCurrentRow(0)

    def init_options_list(self):
        """
        Displays all model settings
        """
        model_setups = self.data["Model setups"].keys()
        for setting_name in model_setups:
            self.model_options_list.addItem(QListWidgetItem(setting_name))
        if len(model_setups) > 0:
            self.model_options_list.setCurrentRow(0)

    def selection_changed(self):
        """
        Checks if both a component list and a model setup has been chosen.
        If so, the user is enabled to proceed
        """
        component_list_item = self.composition_list.currentItem()
        option_mode_item = self.model_options_list.currentItem()

        if not (component_list_item and option_mode_item):
            self.go_btn.setEnabled(False)
            self.close_button.setAutoDefault(True)
        else:
            self.go_btn.setEnabled(True)
            self.go_btn.setAutoDefault(True)


class GoToPlotModeWidget(ChangeModePopup):
    """
    Window popup beofre going to Plot Mode
    The user selects one component list, and one model setup before proceeding
    """
    def __init__(self, data, json_file, parent=None):
        ChangeModePopup.__init__(self, data, json_file, parent)
        self.go_btn.clicked.connect(self.go_to_plot_mode)

    def go_to_plot_mode(self):
        """
        Opens a Plot Mode window, and closes itself
        """
        component_list_name = self.composition_list.currentItem().text()
        model_settings_name = self.model_options_list.currentItem().text()

        self.plot_window = PlotMode(self.data, self.json_file, component_list_name, model_settings_name)
        self.plot_window.show()
        self.close()


class GoToCalcModeWidget(ChangeModePopup):
    """
    Window popup beofre going to Calc Mode
    The user selects one component list, and one model setup before proceeding
    """
    def __init__(self, data, json_file, parent=None):
        ChangeModePopup.__init__(self, data, json_file, parent)
        self.go_btn.clicked.connect(self.go_to_calc_mode)

    def go_to_calc_mode(self):
        """
        Opens a Calc Mode window, and closes itself
        """
        component_list_name = self.composition_list.currentItem().text()
        model_settings_name = self.model_options_list.currentItem().text()

        self.calc_window = CalcMode(self.data, self.json_file, component_list_name, model_settings_name)
        self.calc_window.show()
        self.close()
