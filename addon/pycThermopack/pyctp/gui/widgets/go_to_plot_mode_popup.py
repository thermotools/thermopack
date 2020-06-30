from PyQt5.QtWidgets import QDialog, QListWidgetItem
from PyQt5.uic import loadUi
from gui.widgets.plot_mode import PlotMode


# TODO: Når du dobbelttrykker på en complist eller setting (ev. hover) --> Vise vindu med info (ev. et (?)-ikon)

class GoToPlotModeWidget(QDialog):
    def __init__(self, data, parent=None):
        super().__init__(parent=parent)
        loadUi("widgets/layouts/go_to_plot_mode_popup.ui", self)
        self.setWindowTitle("Thermopack")

        self.data = data

        self.init_composition_list()
        self.init_options_list()

        if self.composition_list.currentItem() and self.model_options_list.currentItem():
            self.go_to_plot_mode_button.setEnabled(True)
            self.go_to_plot_mode_button.setAutoDefault(True)
        else:
            self.close_button.setAutoDefault(True)

        self.composition_list.itemSelectionChanged.connect(self.selection_changed)
        self.model_options_list.itemSelectionChanged.connect(self.selection_changed)

        self.close_button.clicked.connect(lambda: self.close())
        self.go_to_plot_mode_button.clicked.connect(self.go_to_plot_mode)

    def init_composition_list(self):
        component_lists = self.data["Component lists"].keys()
        for composition_name in component_lists:
            self.composition_list.addItem(QListWidgetItem(composition_name))
        if len(component_lists) > 0:
            self.composition_list.setCurrentRow(0)

    def init_options_list(self):
        model_setups = self.data["Model setups"].keys()
        for setting_name in model_setups:
            self.model_options_list.addItem(QListWidgetItem(setting_name))
        if len(model_setups) > 0:
            self.model_options_list.setCurrentRow(0)

    def selection_changed(self):
        component_list_item = self.composition_list.currentItem()
        option_mode_item = self.model_options_list.currentItem()

        if not (component_list_item and option_mode_item):
            self.go_to_plot_mode_button.setEnabled(False)
            self.close_button.setAutoDefault(True)
        else:
            self.go_to_plot_mode_button.setEnabled(True)
            self.go_to_plot_mode_button.setAutoDefault(True)

    def go_to_plot_mode(self):
        component_list_name = self.composition_list.currentItem().text()
        model_settings_name = self.model_options_list.currentItem().text()

        settings = self.data["Model setups"][model_settings_name]
        component_data = self.data["Component lists"][component_list_name]

        self.plot_window = PlotMode(component_data, settings)
        self.plot_window.show()
        self.close()
