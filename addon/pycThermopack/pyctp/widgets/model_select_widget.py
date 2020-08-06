from PyQt5.QtWidgets import QMessageBox, QTreeWidgetItem
from PyQt5.uic import loadUi
from PyQt5.QtCore import pyqtSignal, Qt

from gui.widgets.parameters import *

from gui.gui_utils import get_unique_name, get_unique_id

# TODO: Mulighet for å slette en model setup


class ModelListMenuItem(QTreeWidgetItem):
    """
    A TreeWidgetItem which also contains an id and a widget.
    This connects the menu item to the correct ModelSelectWidget, making it possible to open an edit tab corresponing
    to the chosen model setting.
    """

    def __init__(self, parent, text, id, widget):
        super().__init__(parent, [text])
        self.id = id
        self.widget = widget


class ModelSelectWidget(QWidget):
    """
    Widget for selecting and setting up models. The user can select the model category, the desired EOS,
    set specific model options and edit interaction (and other) parameters.
    Depending on the selcted model, different choices for EOS, model options and parameters are displayed.
    The different choices are stored in QStackWidgets, each with its own index shown below. When the user changes
    the model category, the correct widgets in the different stacks are shown.
    EOS stack:      0: Empty   1: PR + SRK
    Options stack:  0: Empty   1: Cubic (or CPA)    2: SAFT-VRI Mie
    Coeff stack:    0: Empty   1: VdW               2: HV1              3: HV2        4: PC-SAFT        5: SAFT-VR Mie
    """

    def __init__(self, data, name=None, parent=None):
        QWidget.__init__(self, parent)

        loadUi("widgets/layouts/model_select_widget.ui", self)

        self.data = data
        if name:
            self.name = name
            self.init_bin_coeff_widgets()
            self.populate_widget()

        else:
            self.name = get_unique_name("Model setup", self.data["Model setups"].keys())
            self.name_edit.setPlaceholderText(self.name)
            self.data_init()
            self.init_bin_coeff_widgets()

        self.id = self.data["Model setups"][self.name]["id"]

        self.settings_updated.emit(self.name, self.data, self.id)

        self.model_category_list.currentItemChanged.connect(self.category_selected)

        # Cubic Action handling
        self.cubic_eos_list.currentItemChanged.connect(self.change_cubic_eos)

        self.cubic_alpha_corr.currentTextChanged.connect(self.change_cubic_alpha_corr)
        self.cubic_mix_rule.currentTextChanged.connect(self.change_cubic_mix_rule)
        self.cubic_vol_trans.currentTextChanged.connect(self.change_cubic_vol_trans)
        self.cubic_ref.currentTextChanged.connect(self.change_cubic_ref)

        # SAFT-VR Mie Action handling
        self.saftvrmie_hard_sphere.clicked.connect(self.toggle_hard_sphere)
        self.saftvrmie_a1.clicked.connect(self.toggle_a1)
        self.saftvrmie_a2.clicked.connect(self.toggle_a2)
        self.saftvrmie_a3.clicked.connect(self.toggle_a3)
        self.saftvrmie_chain.clicked.connect(self.toggle_chain)

        self.tabs.currentChanged.connect(self.show_correct_tab)

        self.name_edit.editingFinished.connect(self.change_name)

    settings_updated = pyqtSignal(str, dict, int)

    def populate_widget(self):
        """
        Sets the widget with the correct settings for an existing model setup. The correct widgets in the different
        stacks are shown.
        """
        self.name_edit.setText(self.name)
        model_category = self.data["Model setups"][self.name]["Model category"]
        self.model_category_list.setCurrentItem(self.model_category_list.findItems(model_category, Qt.MatchExactly)[0])

        if model_category == "Cubic" or model_category == "CPA":
            self.eos_stack.setCurrentIndex(1)
            self.options_stack.setCurrentIndex(1)

            eos = self.data["Model setups"][self.name]["EOS"]
            alpha_corr = self.data["Model setups"][self.name]["Model options"]["Alpha correlation"]
            mixing_rule = self.data["Model setups"][self.name]["Model options"]["Mixing rule"]
            vol_trans = self.data["Model setups"][self.name]["Model options"]["Volume translation"]
            ref = self.data["Model setups"][self.name]["Model options"]["Reference"]

            self.cubic_eos_list.setCurrentItem(self.cubic_eos_list.findItems(eos, Qt.MatchExactly)[0])
            self.cubic_alpha_corr.setCurrentIndex(self.cubic_alpha_corr.findText(alpha_corr))
            self.cubic_mix_rule.setCurrentIndex(self.cubic_mix_rule.findText(mixing_rule))
            self.cubic_vol_trans.setCurrentIndex(self.cubic_vol_trans.findText(vol_trans))
            self.cubic_ref.setCurrentIndex(self.cubic_ref.findText(ref))

        elif model_category == "PC-SAFT":
            self.eos_stack.setCurrentIndex(0)
            self.options_stack.setCurrentIndex(0)

        elif model_category == "SAFT-VR Mie":
            self.eos_stack.setCurrentIndex(0)
            self.options_stack.setCurrentIndex(2)

            a1 = self.data["Model setups"][self.name]["Model options"]["A1"]
            a2 = self.data["Model setups"][self.name]["Model options"]["A2"]
            a3 = self.data["Model setups"][self.name]["Model options"]["A3"]
            hard_sphere = self.data["Model setups"][self.name]["Model options"]["Hard sphere"]
            chain = self.data["Model setups"][self.name]["Model options"]["Chain"]
            ref = self.data["Model setups"][self.name]["Model options"]["Reference"]

            self.saftvrmie_a1.setChecked(a1)
            self.saftvrmie_a2.setChecked(a2)
            self.saftvrmie_a3.setChecked(a3)
            self.saftvrmie_hard_sphere.setChecked(hard_sphere)
            self.saftvrmie_chain.setChecked(chain)
            self.saftvrmie_ref.setCurrentIndex(self.saftvrmie_ref.findText(ref))

        self.show_correct_coeff_widget()

    def init_bin_coeff_widgets(self):
        """
        Sets up widgets for editing parameters and storing their indices, so that the correct widget easily can be
        shown when the model category or mixing rule changes
        """
        # Set binary coefficient widgets
        self.vdw_index = self.coeff_stack.addWidget(VdWParametersWidget(self.data, self.name))
        self.hv1_index = self.coeff_stack.addWidget(HV1ParametersWidget(self.data, self.name))
        self.hv2_index = self.coeff_stack.addWidget(HV2ParametersWidget(self.data, self.name))
        self.pcsaft_index = self.coeff_stack.addWidget(PCSAFTParametersWidget(self.data, self.name))
        self.saftvrmie_index = self.coeff_stack.addWidget(SAFTVRMieParametersWidget(self.data, self.name))
        self.show_correct_coeff_widget()

    def data_init(self, category="Cubic"):
        """
        Sets the session data to default depending on chosen model category if this is a new model setup
        :param category: Name of the model category (Cubic, CPA, PC-SAFT, SAFT_VR Mie)
        """
        if category == "Cubic" or category == "CPA":
            self.data["Model setups"][self.name] = {
                "EOS": "PR",
                "id": get_unique_id(self.data),
                "Model category": category,
                "Model options": {
                    "Alpha correlation": "Classic",
                    "Mixing rule": "vdW",
                    "Reference": "Default",
                    "Volume translation": "None"
                },
                "Parameters": {}
            }

        elif category == "PC-SAFT":
            self.data["Model setups"][self.name] = {
                "id": get_unique_id(self.data),
                "Model category": category,
                "Model options": {
                    "Reference": "Default"
                },
                "Parameters": {}
            }

        elif category == "SAFT-VR Mie":
            self.data["Model setups"][self.name] = {
                "id": get_unique_id(self.data),
                "Model options": {
                    "Hard sphere": True,
                    "A1": True,
                    "A2": True,
                    "A3": True,
                    "Chain": True,
                    "Reference": "Default"
                },
                "Parameters": {}
            }

    def show_correct_coeff_widget(self):
        """
        Shows the correct widget for editing interaction (and other) parameters
        """
        category = self.data["Model setups"][self.name]["Model category"]
        if category in ["Cubic", "CPA"]:
            mixing_rule = self.data["Model setups"][self.name]["Model options"]["Mixing rule"]
            if mixing_rule == "vdW":
                index = self.vdw_index
            elif mixing_rule == "HV1":
                index = self.hv1_index
            elif mixing_rule == "HV2":
                index = self.hv2_index
            else:
                self.coeff_stack.setCurrentIndex(0)
                return

        elif category == "PC-SAFT":
            index = self.pcsaft_index

        elif category == "SAFT-VR Mie":
            index = self.saftvrmie_index

        else:
            index = None

        if index:
            self.coeff_stack.setCurrentIndex(index)

    def category_selected(self, category_item):
        """
        Shows the correct widgets in the different stacks when a model category is chosen.
        :param category_item: Chosen model category item
        """
        category = category_item.text()
        if not self.data["Model setups"][self.name]["Model category"]:
            self.data_init(category)
        self.data["Model setups"][self.name]["Model category"] = category
        if category == "Cubic" or category == "CPA":
            self.eos_stack.setCurrentIndex(1)
            self.options_stack.setCurrentIndex(1)

            # Store default choices
            self.change_cubic_eos(self.cubic_eos_list.currentItem())
            self.change_cubic_alpha_corr(self.cubic_alpha_corr.currentText())
            self.change_cubic_mix_rule(self.cubic_mix_rule.currentText())
            self.change_cubic_vol_trans(self.cubic_vol_trans.currentText())
            self.change_cubic_ref(self.cubic_ref.currentText())

        elif category == "PC-SAFT":
            self.eos_stack.setCurrentIndex(0)
            self.options_stack.setCurrentIndex(0)

        elif category == "SAFT-VR Mie":
            self.eos_stack.setCurrentIndex(0)
            self.options_stack.setCurrentIndex(2)

            self.toggle_a1(self.saftvrmie_a1.isChecked())
            self.toggle_a2(self.saftvrmie_a2.isChecked())
            self.toggle_a3(self.saftvrmie_a3.isChecked())
            self.toggle_hard_sphere(self.saftvrmie_hard_sphere.isChecked())
            self.toggle_chain(self.saftvrmie_chain.isChecked())
            self.change_saftvrmie_ref(self.saftvrmie_ref.currentText())

        else:
            pass

        self.show_correct_coeff_widget()

    # Handling changes in cubic model options

    def change_cubic_eos(self, eos_item):
        eos = eos_item.text()
        self.data["Model setups"][self.name]["EOS"] = eos
        self.settings_updated.emit(self.name, self.data, self.id)

    def change_cubic_alpha_corr(self, alpha_corr):
        self.data["Model setups"][self.name]["Model options"]["Alpha correlation"] = alpha_corr
        self.settings_updated.emit(self.name, self.data, self.id)

    def change_cubic_mix_rule(self, mixing_rule):
        self.data["Model setups"][self.name]["Model options"]["Mixing rule"] = mixing_rule
        self.settings_updated.emit(self.name, self.data, self.id)
        self.show_correct_coeff_widget()

    def change_cubic_vol_trans(self, vol_trans):
        self.data["Model setups"][self.name]["Model options"]["Volume translation"] = vol_trans
        self.settings_updated.emit(self.name, self.data, self.id)

    def change_cubic_ref(self, ref):
        self.data["Model setups"][self.name]["Model options"]["Reference"] = ref
        self.settings_updated.emit(self.name, self.data, self.id)

    # Handling changes in SAFT-VR Mie options

    def toggle_hard_sphere(self, is_checked):
        self.data["Model setups"][self.name]["Model options"]["Hard sphere"] = is_checked

    def toggle_a1(self, is_checked):
        self.data["Model setups"][self.name]["Model options"]["A1"] = is_checked

    def toggle_a2(self, is_checked):
        self.data["Model setups"][self.name]["Model options"]["A2"] = is_checked

    def toggle_a3(self, is_checked):
        self.data["Model setups"][self.name]["Model options"]["A3"] = is_checked

    def toggle_chain(self, is_checked):
        self.data["Model setups"][self.name]["Model options"]["Chain"] = is_checked

    def change_saftvrmie_ref(self, ref):
        self.data["Model setups"][self.name]["Model options"]["Reference"] = ref

    def show_correct_tab(self, index):
        """
        When the 'Parameters' tab is chosen, the current coefficient widget is initiated
        :param index: Index of the clicked tab
        """
        if index >= 0:
            if self.tabs.tabText(index) == "Parameters":
                self.coeff_stack.currentWidget().init_widget(self.data, self.name)

    def change_name(self):
        """
        Changes the name of the current model setup if the name is available
        """
        new_name = self.name_edit.text()
        self.name_edit.blockSignals(True)
        self.name_edit.clearFocus()
        self.name_edit.blockSignals(False)

        if new_name in self.data["Model setups"] and new_name != self.name:
            msg = SettingsExistMsg(new_name)
            msg.exec_()
            self.name_edit.undo()

        else:
            self.data["Model setups"][new_name] = self.data["Model setups"].pop(self.name)
            self.name = new_name
            self.settings_updated.emit(self.name, self.data, self.id)


class SettingsExistMsg(QMessageBox):
    """
    Showed when the user tries to save a model setup with the name of another model setup
    """

    def __init__(self, name):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("%s already exists." % name)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
