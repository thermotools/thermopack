from PyQt5.QtWidgets import QWidget, QMessageBox, QTreeWidgetItem
from PyQt5.uic import loadUi
from PyQt5.QtCore import pyqtSignal, Qt

from gui.widgets.mixing_rules import *

from gui.utils import get_unique_name, get_unique_id

# TODO
#  Ordne mer i menyen: PushButton for hovedmenyene med ikon for å lage ny

# TODO:  No self associating components. Initializing PR(/SRK) instead of CPA-PR(/SRK) dukker opp hele tiden for CPA


class ModelListMenuItem(QTreeWidgetItem):
    def __init__(self, parent, text, id, widget):
        super().__init__(parent, [text])
        self.id = id
        self.widget = widget


class ModelSelectWidget(QWidget):
    """
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
        self.name_edit.setText(self.name)
        model_category = self.data["Model setups"][self.name]["Model category"]
        self.model_category_list.setCurrentItem(self.model_category_list.findItems(model_category, Qt.MatchExactly)[0])

        if model_category == "Cubic" or model_category == "CPA":
            eos = self.data["Model setups"][self.name]["EOS"]
            alpha_corr = self.data["Model setups"][self.name]["Model options"]["Alpha correlation"]
            mixing_rule = self.data["Model setups"][self.name]["Model options"]["Mixing rule"]
            vol_trans = self.data["Model setups"][self.name]["Model options"]["Volume translation"]
            ref = self.data["Model setups"][self.name]["Model options"]["Reference"]

            self.show_correct_coeff_widget()

            self.cubic_eos_list.setCurrentItem(self.cubic_eos_list.findItems(eos, Qt.MatchExactly)[0])
            self.cubic_alpha_corr.setCurrentIndex(self.cubic_alpha_corr.findText(alpha_corr))
            self.cubic_mix_rule.setCurrentIndex(self.cubic_mix_rule.findText(mixing_rule))
            self.cubic_vol_trans.setCurrentIndex(self.cubic_vol_trans.findText(vol_trans))
            self.cubic_ref.setCurrentIndex(self.cubic_ref.findText(ref))

        elif model_category == "PC-SAFT":
            pass

        elif model_category == "SAFT-VR Mie":
            pass

    def init_bin_coeff_widgets(self):
        # Set binary coefficient widgets
        self.vdw_index = self.coeff_stack.addWidget(VdWBinaryCoefficientsWidget(self.data, self.name))
        self.hv1_index = self.coeff_stack.addWidget(HV1BinaryCoefficientsWidget(self.data, self.name))
        self.hv2_index = self.coeff_stack.addWidget(HV2BinaryCoefficientsWidget(self.data, self.name))
        self.pcsaft_index = self.coeff_stack.addWidget(PCSAFTBinaryCoefficientsWidget(self.data, self.name))
        self.saftvrmie_index = self.coeff_stack.addWidget(SAFTVRMieBinaryCoefficientsWidget(self.data, self.name))
        self.show_correct_coeff_widget()

    def data_init(self, category="Cubic"):
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
                }
            }

        elif category == "PC-SAFT":
            self.data["Model setups"][self.name] = {
                "id": get_unique_id(self.data),
                "Model category": category,
                "Model options": {
                    "Reference": "Default"
                }
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
                }
            }

    def show_correct_coeff_widget(self):
        category = self.data["Model setups"][self.name]["Model category"]
        if category == "Cubic" or category == "CPA":
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
        # coeff_widget = self.coeff_stack.widget(index)

    def category_selected(self, category_item):
        category = category_item.text()
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
        else:
            pass

        self.show_correct_coeff_widget()

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

    def change_cubic_vol_trans(self, vol_trans):
        self.data["Model setups"][self.name]["Model options"]["Volume translation"] = vol_trans
        self.settings_updated.emit(self.name, self.data, self.id)

    def change_cubic_ref(self, ref):
        self.data["Model setups"][self.name]["Model options"]["Reference"] = ref
        self.settings_updated.emit(self.name, self.data, self.id)

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

    def show_correct_tab(self, index):
        if index >= 0:
            if self.tabs.tabText(index) == "Binary coefficients":
                self.coeff_stack.currentWidget().init_widget(self.data, self.name)

    def change_name(self):
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
    def __init__(self, name):
        QMessageBox.__init__(self)
        self.setWindowTitle("Oups!")
        self.setText("%s already exists." % name)
        self.setIcon(QMessageBox.Information)
        self.setStandardButtons(QMessageBox.Close)
        self.setDefaultButton(QMessageBox.Ignore)
