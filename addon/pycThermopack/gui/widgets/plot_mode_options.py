from PyQt5.QtWidgets import QDialog, QColorDialog, QLineEdit
from PyQt5.uic import loadUi
from PyQt5.QtGui import QIntValidator
from PyQt5.QtCore import QLocale

from gui.utils import FloatValidator


class PhaseEnvelopeOptionsWindow(QDialog):
    """
    A window containing the parameters for the phase envelope plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("layouts/ph_env_options.ui", self)
        self.setWindowTitle("Phase envelope options")

        self.plotting_preferences = plotting_preferences
        self.calc_pvt_settings = self.plotting_preferences["Phase envelope"]["TPV"]
        self.isopleth_settings = self.plotting_preferences["Phase envelope"]["Isopleths"]
        self.crit_point_settings = self.plotting_preferences["Phase envelope"]["Critical"]
        self.plotting_options = self.plotting_preferences["Phase envelope"]["Plotting"]

        self.default = default

        # Set initial data
        self.p_0.setText(str(self.calc_pvt_settings["Initial pressure"]))
        self.p_max.setText(str(self.calc_pvt_settings["Maximum pressure"]))
        self.t_min.setText(str(self.calc_pvt_settings["Minimum temperature"]))
        self.step_size.setText(str(self.calc_pvt_settings["Step size"]))

        self.crit_t.setText(str(self.crit_point_settings["Temperature"]))
        self.crit_v.setText(str(self.crit_point_settings["Volume"]))
        self.crit_tol.setText(str(self.crit_point_settings["Error tolerance"]))

        self.iso_p_min.setText(str(self.isopleth_settings["Minimum pressure"]))
        self.iso_p_max.setText(str(self.isopleth_settings["Maximum pressure"]))
        self.iso_t_min.setText(str(self.isopleth_settings["Minimum temperature"]))
        self.iso_t_max.setText(str(self.isopleth_settings["Maximum temperature"]))
        self.n_isopleths.setText(str(self.isopleth_settings["Number of isopleths"]))
        self.n_max.setText(str(self.isopleth_settings["N max"]))

        self.set_line_color(self.plotting_options["Colors"][0])
        self.set_point_color(self.plotting_options["Colors"][1])
        self.set_isopleth_1_color(self.plotting_options["Colors"][2])
        self.set_isopleth_2_color(self.plotting_options["Colors"][3])
        self.grid_checkbox.setChecked(self.plotting_options["Grid on"])
        self.title.setText(self.plotting_options["Title"])
        self.xlabel.setText(self.plotting_options["x label"])
        self.ylabel.setText(self.plotting_options["y label"])

        self.setFocus()

        # Validators for input
        float_validator = FloatValidator()
        int_validator = QIntValidator()

        self.p_0.setValidator(float_validator)
        self.p_max.setValidator(float_validator)
        self.t_min.setValidator(float_validator)
        self.step_size.setValidator(float_validator)

        self.crit_t.setValidator(float_validator)
        self.crit_v.setValidator(float_validator)
        self.crit_tol.setValidator(float_validator)

        self.iso_p_min.setValidator(float_validator)
        self.iso_p_max.setValidator(float_validator)
        self.iso_t_min.setValidator(float_validator)
        self.iso_t_max.setValidator(float_validator)
        self.n_isopleths.setValidator(int_validator)
        self.n_max.setValidator(int_validator)

        # Action handling
        self.line_color_tool_btn.clicked.connect(self.set_line_color)
        self.point_color_tool_btn.clicked.connect(self.set_point_color)
        self.isopleth_1_color_tool_btn.clicked.connect(self.set_isopleth_1_color)
        self.isopleth_2_color_tool_btn.clicked.connect(self.set_isopleth_2_color)

        self.save_btn.clicked.connect(self.save)
        self.cancel_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_line_color(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_preview.setStyleSheet(style)

    def set_point_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.point_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.point_color_preview.setStyleSheet(style)

    def set_isopleth_1_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_1_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_1_color_preview.setStyleSheet(style)

    def set_isopleth_2_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_2_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_2_color_preview.setStyleSheet(style)

    def save(self):
        self.calc_pvt_settings["Initial pressure"] = float(self.p_0.text())
        self.calc_pvt_settings["Maximum pressure"] = float(self.p_max.text())

        if self.t_min.text().lower() != "none":
            self.calc_pvt_settings["Minimum temperature"] = float(self.t_min.text())
        else:
            self.calc_pvt_settings["Minimum temperature"] = None

        self.calc_pvt_settings["Step size"] = float(self.step_size.text())

        self.crit_point_settings["Temperature"] = float(self.crit_t.text())
        self.crit_point_settings["Volume"] = float(self.crit_v.text())
        self.crit_point_settings["Error tolerance"] = float(self.crit_tol.text())

        self.isopleth_settings["Minimum pressure"] = float(self.iso_p_min.text())
        self.isopleth_settings["Maximum pressure"] = float(self.iso_p_max.text())
        self.isopleth_settings["Minimum temperature"] = float(self.iso_t_min.text())
        self.isopleth_settings["Maximum temperature"] = float(self.iso_t_max.text())
        self.isopleth_settings["Number of isopleths"] = int(self.n_isopleths.text())
        self.isopleth_settings["N max"] = int(self.n_max.text())

        self.plotting_options["Colors"][0] = self.line_color_preview.palette().window().color().name()
        self.plotting_options["Colors"][1] = self.point_color_preview.palette().window().color().name()
        self.plotting_options["Colors"][2] = self.isopleth_1_color_preview.palette().window().color().name()
        self.plotting_options["Colors"][3] = self.isopleth_2_color_preview.palette().window().color().name()
        self.plotting_options["Title"] = self.title.text()
        self.plotting_options["x label"] = self.xlabel.text()
        self.plotting_options["y label"] = self.ylabel.text()
        self.plotting_options["Grid on"] = self.grid_checkbox.isChecked()

        self.close()

    def restore_defaults(self):
        calc_pvt_settings = self.default["Phase envelope"]["TPV"]
        isopleth_settings = self.default["Phase envelope"]["Isopleths"]
        crit_point_settings = self.default["Phase envelope"]["Critical"]
        plotting_options = self.default["Phase envelope"]["Plotting"]

        self.p_0.setText(str(calc_pvt_settings["Initial pressure"]))
        self.p_max.setText(str(calc_pvt_settings["Maximum pressure"]))
        self.t_min.setText(str(calc_pvt_settings["Minimum temperature"]))
        self.step_size.setText(str(calc_pvt_settings["Step size"]))

        self.crit_t.setText(str(crit_point_settings["Temperature"]))
        self.crit_v.setText(str(crit_point_settings["Volume"]))
        self.crit_tol.setText(str(crit_point_settings["Error tolerance"]))

        self.iso_p_min.setText(str(isopleth_settings["Minimum pressure"]))
        self.iso_p_max.setText(str(isopleth_settings["Maximum pressure"]))
        self.iso_t_min.setText(str(isopleth_settings["Minimum temperature"]))
        self.iso_t_max.setText(str(isopleth_settings["Maximum temperature"]))
        self.n_isopleths.setText(str(isopleth_settings["Number of isopleths"]))
        self.n_max.setText(str(isopleth_settings["N max"]))

        self.set_line_color(plotting_options["Colors"][0])
        self.set_point_color(plotting_options["Colors"][1])
        self.set_isopleth_1_color(plotting_options["Colors"][2])
        self.set_isopleth_2_color(plotting_options["Colors"][3])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText(plotting_options["Title"])
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])


class BinaryPXYOptionsWindow(QDialog):
    """
    A window containing the parameters for the binary pxy plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("layouts/bin_pxy_options.ui", self)
        self.setWindowTitle("Binary pxy options")

        self.calc_settings = plotting_preferences["Binary pxy"]["Calc"]
        self.plotting_options = plotting_preferences["Binary pxy"]["Plotting"]

        self.default = default

        # Set initial data
        self.temp.setText(str(self.calc_settings["Temperature"]))
        self.p_max.setText(str(self.calc_settings["Maximum pressure"]))
        self.p_min.setText(str(self.calc_settings["Minimum pressure"]))
        self.dz_max.setText(str(self.calc_settings["Maximum dz"]))
        self.dlns_max.setText(str(self.calc_settings["Maximum dlns"]))

        self.set_line_color(self.plotting_options["Colors"][0])
        self.grid_checkbox.setChecked(self.plotting_options["Grid on"])
        self.title.setText(self.plotting_options["Title"])
        self.xlabel.setText(self.plotting_options["x label"])
        self.ylabel.setText(self.plotting_options["y label"])

        self.setFocus()

        # Validators for input
        float_validator = FloatValidator()

        self.temp.setValidator(float_validator)
        self.p_max.setValidator(float_validator)
        self.p_min.setValidator(float_validator)
        self.dz_max.setValidator(float_validator)
        self.dlns_max.setValidator(float_validator)

        # Action handling
        self.line_color_tool_btn.clicked.connect(self.set_line_color)

        self.save_btn.clicked.connect(self.save)
        self.cancel_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_line_color(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_preview.setStyleSheet(style)

    def save(self):
        self.calc_settings["Temperature"] = float(self.temp.text())
        self.calc_settings["Maximum pressure"] = float(self.p_max.text())
        self.calc_settings["Minimum pressure"] = float(self.p_min.text())
        self.calc_settings["Maximum dz"] = float(self.dz_max.text())
        self.calc_settings["Maximum dlns"] = float(self.dlns_max.text())

        self.plotting_options["Colors"][0] = self.line_color_preview.palette().window().color().name()
        self.plotting_options["Grid on"] = self.grid_checkbox.isChecked()
        self.plotting_options["Title"] = self.title.text()
        self.plotting_options["x label"] = self.xlabel.text()
        self.plotting_options["y label"] = self.ylabel.text()

        self.close()

    def restore_defaults(self):
        calc_settings = self.default["Binary pxy"]["Calc"]
        plotting_options = self.default["Binary pxy"]["Plotting"]

        self.temp.setText(str(calc_settings["Temperature"]))
        self.p_max.setText(str(calc_settings["Maximum pressure"]))
        self.p_min.setText(str(calc_settings["Minimum pressure"]))
        self.dz_max.setText(str(calc_settings["Maximum dz"]))
        self.dlns_max.setText(str(calc_settings["Maximum dlns"]))

        self.set_line_color(plotting_options["Colors"][0])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText(plotting_options["Title"])
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])


class PRhoOptionsWindow(QDialog):
    """
    A window containing the parameters for the pressure density plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("layouts/pressure_density_options.ui", self)
        self.setWindowTitle("Pressure density options")

        self.new_temp_btn.setStyleSheet("padding: 3px 7px;")

        self.calc_settings = plotting_preferences["Pressure density"]["Calc"]
        self.tpv_settings = plotting_preferences["Pressure density"]["TPV"]
        self.crit_point_settings = plotting_preferences["Pressure density"]["Critical"]
        self.plotting_options = plotting_preferences["Pressure density"]["Plotting"]

        self.default = default

        # Set initial data
        temperatures = self.calc_settings["Temperatures"]
        self.float_validator = FloatValidator()

        for i in range(len(temperatures)):
            line_edit = QLineEdit()
            line_edit.setValidator(self.float_validator)
            line_edit.setText(str(temperatures[i]))
            self.isotherm_layout.addRow("Temperature " + str(i), line_edit)

        self.v_start.setText(str(self.calc_settings["Volume range start"]))
        self.v_end.setText(str(self.calc_settings["Volume range end"]))
        self.v_num_points.setText(str(self.calc_settings["Num points"]))

        self.p_0.setText(str(self.tpv_settings["Initial pressure"]))
        self.p_max.setText(str(self.tpv_settings["Maximum pressure"]))
        self.t_min.setText(str(self.tpv_settings["Minimum temperature"]))
        self.step_size.setText(str(self.tpv_settings["Step size"]))

        self.crit_t.setText(str(self.crit_point_settings["Temperature"]))
        self.crit_v.setText(str(self.crit_point_settings["Volume"]))
        self.crit_tol.setText(str(self.crit_point_settings["Error tolerance"]))

        self.grid_checkbox.setChecked(self.plotting_options["Grid on"])
        self.title.setText(self.plotting_options["Title"])
        self.xlabel.setText(self.plotting_options["x label"])
        self.ylabel.setText(self.plotting_options["y label"])

        self.setFocus()

        # Setting validators for input
        int_validator = QIntValidator()

        self.v_start.setValidator(self.float_validator)
        self.v_end.setValidator(self.float_validator)
        self.v_num_points.setValidator(int_validator)

        self.p_0.setValidator(self.float_validator)
        self.p_max.setValidator(self.float_validator)
        self.t_min.setValidator(self.float_validator)
        self.step_size.setValidator(self.float_validator)

        self.crit_t.setValidator(self.float_validator)
        self.crit_v.setValidator(self.float_validator)
        self.crit_tol.setValidator(self.float_validator)

        # Action handling
        self.new_temp_btn.clicked.connect(self.add_new_temperature)
        self.save_btn.clicked.connect(self.save)
        self.cancel_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def add_new_temperature(self):
        temperature_number = self.isotherm_layout.rowCount() - 1
        line_edit = QLineEdit()
        line_edit.setValidator(self.float_validator)
        line_edit.setText("298.0")
        self.isotherm_layout.addRow("Temperature " + str(temperature_number), line_edit)
        line_edit.selectAll()
        line_edit.setFocus()

    def set_isopleth_1_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_1_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_1_color_preview.setStyleSheet(style)

    def set_isopleth_2_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_2_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_2_color_preview.setStyleSheet(style)

    def save(self):
        temperatures = []
        col_count = 1
        for row_count in range(1, self.isotherm_layout.rowCount()):
            t_line_edit = self.isotherm_layout.itemAt(row_count, col_count).widget()
            temperatures.append(float(t_line_edit.text()))
        self.calc_settings["Temperatures"] = temperatures

        self.calc_settings["Volume range start"] = float(self.v_start.text())
        self.calc_settings["Volume range end"] = float(self.v_end.text())
        self.calc_settings["Num points"] = int(self.v_num_points.text())

        self.tpv_settings["Initial pressure"] = float(self.p_0.text())
        self.tpv_settings["Maximum pressure"] = float(self.p_max.text())

        if self.t_min.text() != "None":
            self.tpv_settings["Minimum temperature"] = float(self.t_min.text())
        else:
            self.tpv_settings["Minimum temperature"] = None

        self.tpv_settings["Step size"] = float(self.step_size.text())

        self.crit_point_settings["Temperature"] = float(self.crit_t.text())
        self.crit_point_settings["Volume"] = float(self.crit_v.text())
        self.crit_point_settings["Error tolerance"] = float(self.crit_tol.text())

        self.plotting_options["Title"] = self.title.text()
        self.plotting_options["x label"] = self.xlabel.text()
        self.plotting_options["y label"] = self.ylabel.text()
        self.plotting_options["Grid on"] = self.grid_checkbox.isChecked()

        self.close()

    def restore_defaults(self):
        calc_settings = self.default["Pressure density"]["Calc"]
        tpv_settings = self.default["Pressure density"]["TPV"]
        crit_point_settings = self.default["Pressure density"]["Critical"]
        plotting_options = self.default["Pressure density"]["Plotting"]

        for row_count in range(self.isotherm_layout.rowCount() - 1, 0, -1):
            t_line_edit = self.isotherm_layout.itemAt(row_count, 1).widget()
            if row_count == 1:
                t_line_edit.setText(str(calc_settings["Temperatures"][0]))
            else:
                self.isotherm_layout.removeRow(t_line_edit)

        self.v_start.setText(str(calc_settings["Volume range start"]))
        self.v_end.setText(str(calc_settings["Volume range end"]))
        self.v_num_points.setText(str(calc_settings["Num points"]))

        self.p_0.setText(str(tpv_settings["Initial pressure"]))
        self.p_max.setText(str(tpv_settings["Maximum pressure"]))
        self.t_min.setText(str(tpv_settings["Minimum temperature"]))
        self.step_size.setText(str(tpv_settings["Step size"]))

        self.crit_t.setText(str(crit_point_settings["Temperature"]))
        self.crit_v.setText(str(crit_point_settings["Volume"]))
        self.crit_tol.setText(str(crit_point_settings["Error tolerance"]))

        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText(plotting_options["Title"])
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])


class GlobalBinaryOptionsWindow(QDialog):
    """
    A window containing the parameters for the global binary plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("layouts/global_binary_options.ui", self)
        self.setWindowTitle("Global binary options")

        self.calc_settings = plotting_preferences["Global binary"]["Calc"]
        self.plotting_options = plotting_preferences["Global binary"]["Plotting"]

        self.default = default

        # Set initial data
        self.p_min.setText(str(self.calc_settings["Minimum pressure"]))
        self.t_min.setText(str(self.calc_settings["Minimum temperature"]))
        self.azeotropes_checkbox.setChecked(self.calc_settings["Azeotropes"])

        self.set_line_color_1(self.plotting_options["Colors"][0])
        self.set_line_color_2(self.plotting_options["Colors"][1])
        self.set_line_color_3(self.plotting_options["Colors"][2])
        self.set_line_color_4(self.plotting_options["Colors"][3])
        self.grid_checkbox.setChecked(self.plotting_options["Grid on"])
        if not self.plotting_options["Title"]:
            self.plotting_options["Title"] = "van Konyenburg and Scott type: "
        self.title.setText(self.plotting_options["Title"])
        self.xlabel.setText(self.plotting_options["x label"])
        self.ylabel.setText(self.plotting_options["y label"])

        self.setFocus()

        # Validators for input
        float_validator = FloatValidator()
        locale = QLocale(QLocale.English)
        float_validator.setLocale(locale)

        self.p_min.setValidator(float_validator)
        self.t_min.setValidator(float_validator)

        # Action handling
        self.line_color_1_tool_btn.clicked.connect(self.set_line_color_1)
        self.line_color_2_tool_btn.clicked.connect(self.set_line_color_2)
        self.line_color_3_tool_btn.clicked.connect(self.set_line_color_3)
        self.line_color_4_tool_btn.clicked.connect(self.set_line_color_4)

        self.save_btn.clicked.connect(self.save)
        self.cancel_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_line_color_1(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_1_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_1_preview.setStyleSheet(style)

    def set_line_color_2(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_2_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_2_preview.setStyleSheet(style)

    def set_line_color_3(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_3_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_3_preview.setStyleSheet(style)

    def set_line_color_4(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_4_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_4_preview.setStyleSheet(style)

    def save(self):
        self.calc_settings["Minimum pressure"] = float(self.p_min.text())
        self.calc_settings["Minimum temperature"] = float(self.t_min.text())
        self.calc_settings["Azeotropes"] = self.azeotropes_checkbox.isChecked()

        self.plotting_options["Colors"][0] = self.line_color_1_preview.palette().window().color().name()
        self.plotting_options["Colors"][1] = self.line_color_2_preview.palette().window().color().name()
        self.plotting_options["Colors"][2] = self.line_color_3_preview.palette().window().color().name()
        self.plotting_options["Colors"][3] = self.line_color_4_preview.palette().window().color().name()

        self.plotting_options["Title"] = self.title.text()
        self.plotting_options["x label"] = self.xlabel.text()
        self.plotting_options["y label"] = self.ylabel.text()
        self.plotting_options["Grid on"] = self.grid_checkbox.isChecked()

        self.close()

    def restore_defaults(self):
        calc_settings = self.default["Global binary"]["Calc"]
        plotting_options = self.default["Global binary"]["Plotting"]

        self.p_min.setText(str(calc_settings["Minimum pressure"]))
        self.t_min.setText(str(calc_settings["Minimum temperature"]))
        self.azeotropes_checkbox.setChecked(calc_settings["Azeotropes"])

        self.set_line_color_1(plotting_options["Colors"][0])
        self.set_line_color_2(plotting_options["Colors"][1])
        self.set_line_color_3(plotting_options["Colors"][2])
        self.set_line_color_4(plotting_options["Colors"][3])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText("van Konyenburg and Scott type: ")
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])
