from PyQt5.QtWidgets import QDialog, QColorDialog
from PyQt5.uic import loadUi
from PyQt5.QtGui import QDoubleValidator, QIntValidator
from PyQt5.QtCore import QLocale

from gui.utils import valid_float_input


class PhaseEnvelopeOptionsWindow(QDialog):
    """
    A window containing the parameters for the phase envelope plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("widgets/layouts/ph_env_options.ui", self)
        self.setWindowTitle("Phase envelope options")

        self.calc_pvt_settings = plotting_preferences["Phase envelope"]["TPV"]
        self.isopleth_settings = plotting_preferences["Phase envelope"]["Isopleths"]
        self.crit_point_settings = plotting_preferences["Phase envelope"]["Critical"]
        self.plotting_options = plotting_preferences["Phase envelope"]["Plotting"]

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
        float_validator = QDoubleValidator()
        # Set to English to allow dot as decimal point
        locale = QLocale(QLocale.English)
        float_validator.setLocale(locale)
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
        self.p_0.editingFinished.connect(self.set_p_0)
        self.p_max.editingFinished.connect(self.set_p_max)
        self.t_min.editingFinished.connect(self.set_t_min)
        self.step_size.editingFinished.connect(self.set_step_size)

        self.crit_t.editingFinished.connect(self.set_crit_t)
        self.crit_v.editingFinished.connect(self.set_crit_v)
        self.crit_tol.editingFinished.connect(self.set_crit_tol)

        self.iso_p_min.editingFinished.connect(self.set_iso_p_min)
        self.iso_p_max.editingFinished.connect(self.set_iso_p_max)
        self.iso_t_min.editingFinished.connect(self.set_iso_t_min)
        self.iso_t_max.editingFinished.connect(self.set_iso_t_max)
        self.n_isopleths.editingFinished.connect(self.set_n_isopleths)
        self.n_max.editingFinished.connect(self.set_n_max)

        self.line_color_tool_btn.clicked.connect(self.set_line_color)
        self.point_color_tool_btn.clicked.connect(self.set_point_color)
        self.isopleth_1_color_tool_btn.clicked.connect(self.set_isopleth_1_color)
        self.isopleth_2_color_tool_btn.clicked.connect(self.set_isopleth_2_color)
        self.grid_checkbox.clicked.connect(self.set_grid)
        self.title.editingFinished.connect(self.set_plot_title)
        self.xlabel.editingFinished.connect(self.set_plot_x_label)
        self.ylabel.editingFinished.connect(self.set_plot_y_label)

        self.save_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_p_0(self):
        p_0 = self.p_0.text().replace(",", ".")
        if valid_float_input(p_0):
            self.calc_pvt_settings["Initial pressure"] = float(p_0)
            self.p_0.setText(p_0)
        else:
            self.p_0.undo()

    def set_p_max(self):
        p_max = self.p_max.text().replace(",", ".")
        if valid_float_input(p_max):
            self.calc_pvt_settings["Maximum pressure"] = float(p_max)
            self.p_max.setText(p_max)
        else:
            self.p_max.undo()

    def set_t_min(self):
        t_min = self.t_min.text().replace(",", ".")
        if valid_float_input(t_min):
            self.calc_pvt_settings["Minimum temperature"] = float(t_min)
            self.t_min.setText(t_min)
        else:
            self.t_min.undo()

    def set_step_size(self):
        step_size = self.step_size.text().replace(",", ".")
        if valid_float_input(step_size):
            self.calc_pvt_settings["Step size"] = float(step_size)
            self.step_size.setText(step_size)
        else:
            self.step_size.undo()

    def set_crit_t(self):
        crit_t = self.crit_t.text().replace(",", ".")
        if valid_float_input(crit_t):
            self.crit_point_settings["Temperature"] = float(crit_t)
            self.crit_t.setText(crit_t)
        else:
            self.crit_t.undo()

    def set_crit_v(self):
        crit_v = self.crit_v.text().replace(",", ".")
        if valid_float_input(crit_v):
            self.crit_point_settings["Volume"] = float(crit_v)
            self.crit_v.setText(crit_v)
        else:
            self.crit_v.undo()

    def set_crit_tol(self):
        crit_tol = self.crit_tol.text().replace(",", ".")
        if valid_float_input(crit_tol):
            self.crit_point_settings["Error tolerance"] = float(crit_tol)
            self.crit_tol.setText(crit_tol)
        else:
            self.crit_tol.undo()

    def set_iso_p_min(self):
        iso_p_min = self.iso_p_min.text().replace(",", ".")
        if valid_float_input(iso_p_min):
            self.isopleth_settings["Minimum pressure"] = float(iso_p_min)
            self.iso_p_min.setText(iso_p_min)
        else:
            self.iso_p_min.undo()

    def set_iso_p_max(self):
        iso_p_max = self.iso_p_max.text().replace(",", ".")
        if valid_float_input(iso_p_max):
            self.isopleth_settings["Maximum pressure"] = float(iso_p_max)
            self.iso_p_max.setText(iso_p_max)
        else:
            self.iso_p_max.undo()

    def set_iso_t_min(self):
        iso_t_min = self.iso_t_min.text().replace(",", ".")
        if valid_float_input(iso_t_min):
            self.isopleth_settings["Minimum temperature"] = float(iso_t_min)
            self.iso_t_min.setText(iso_t_min)
        else:
            self.iso_t_min.undo()

    def set_iso_t_max(self):
        iso_t_max = self.iso_t_max.text().replace(",", ".")
        if valid_float_input(iso_t_max):
            self.isopleth_settings["Maximum temperature"] = float(iso_t_max)
            self.iso_t_max.setText(iso_t_max)
        else:
            self.iso_t_max.undo()

    def set_n_isopleths(self):
        n_isopleths = self.n_isopleths.text()
        self.isopleth_settings["Number of isopleths"] = int(n_isopleths)

    def set_n_max(self):
        n_max = self.n_max.text()
        self.isopleth_settings["N max"] = int(n_max)

    def set_line_color(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][0] = color

    def set_point_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.point_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.point_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][1] = color

    def set_isopleth_1_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_1_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_1_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][2] = color

    def set_isopleth_2_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_2_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_2_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][3] = color

    def set_grid(self, is_checked):
        self.plotting_options["Grid on"] = is_checked

    def set_plot_title(self):
        title = self.title.text()
        self.plotting_options["Title"] = title

    def set_plot_x_label(self):
        xlabel = self.xlabel.text()
        self.plotting_options["x label"] = xlabel

    def set_plot_y_label(self):
        ylabel = self.ylabel.text()
        self.plotting_options["y label"] = ylabel

    def restore_defaults(self):
        calc_pvt_settings = self.default["Phase envelope"]["TPV"]
        isopleth_settings = self.default["Phase envelope"]["Isopleths"]
        crit_point_settings = self.default["Phase envelope"]["Critical"]
        plotting_options = self.default["Phase envelope"]["Plotting"]

        self.p_0.setText(str(calc_pvt_settings["Initial pressure"]))
        self.p_max.setText(str(calc_pvt_settings["Maximum pressure"]))
        self.t_min.setText(str(calc_pvt_settings["Minimum temperature"]))
        self.step_size.setText(str(calc_pvt_settings["Step size"]))

        self.set_p_0()
        self.set_p_max()
        self.set_t_min()
        self.set_step_size()

        self.crit_t.setText(str(crit_point_settings["Temperature"]))
        self.crit_v.setText(str(crit_point_settings["Volume"]))
        self.crit_tol.setText(str(crit_point_settings["Error tolerance"]))

        self.set_crit_t()
        self.set_crit_v()
        self.set_crit_tol()

        self.iso_p_min.setText(str(isopleth_settings["Minimum pressure"]))
        self.iso_p_max.setText(str(isopleth_settings["Maximum pressure"]))
        self.iso_t_min.setText(str(isopleth_settings["Minimum temperature"]))
        self.iso_t_max.setText(str(isopleth_settings["Maximum temperature"]))
        self.n_isopleths.setText(str(isopleth_settings["Number of isopleths"]))
        self.n_max.setText(str(isopleth_settings["N max"]))

        self.set_iso_p_min()
        self.set_iso_p_max()
        self.set_iso_t_min()
        self.set_iso_t_max()
        self.set_n_isopleths()
        self.set_n_max()

        self.set_line_color(plotting_options["Colors"][0])
        self.set_point_color(plotting_options["Colors"][1])
        self.set_isopleth_1_color(plotting_options["Colors"][2])
        self.set_isopleth_2_color(plotting_options["Colors"][3])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText(plotting_options["Title"])
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])

        self.set_grid(self.grid_checkbox.isChecked())
        self.set_plot_title()
        self.set_plot_x_label()
        self.set_plot_y_label()


class BinaryPXYOptionsWindow(QDialog):
    """
    A window containing the parameters for the binary pxy plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("widgets/layouts/bin_pxy_options.ui", self)
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
        float_validator = QDoubleValidator()
        locale = QLocale(QLocale.English)
        float_validator.setLocale(locale)

        self.temp.setValidator(float_validator)
        self.p_max.setValidator(float_validator)
        self.p_min.setValidator(float_validator)
        self.dz_max.setValidator(float_validator)
        self.dlns_max.setValidator(float_validator)

        # Action handling

        self.temp.editingFinished.connect(self.set_temp)
        self.p_max.editingFinished.connect(self.set_p_max)
        self.p_min.editingFinished.connect(self.set_p_min)
        self.dz_max.editingFinished.connect(self.set_dz_max)
        self.dlns_max.editingFinished.connect(self.set_dlns_max)

        self.line_color_tool_btn.clicked.connect(self.set_line_color)
        self.grid_checkbox.clicked.connect(self.set_grid)
        self.title.editingFinished.connect(self.set_plot_title)
        self.xlabel.editingFinished.connect(self.set_plot_x_label)
        self.ylabel.editingFinished.connect(self.set_plot_y_label)

        self.save_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_temp(self):
        temp = self.temp.text().replace(",", ".")
        if valid_float_input(temp):
            self.calc_settings["Temperature"] = float(temp)
            self.temp.setText(temp)
        else:
            self.temp.undo()

    def set_p_max(self):
        p_max = self.p_max.text().replace(",", ".")
        if valid_float_input(p_max):
            self.calc_settings["Maximum pressure"] = float(p_max)
            self.p_max.setText(p_max)
        else:
            self.p_max.undo()

    def set_p_min(self):
        p_min = self.p_min.text().replace(",", ".")
        if valid_float_input(p_min):
            self.calc_settings["Minimum pressure"] = float(p_min)
            self.p_min.setText(p_min)
        else:
            self.p_min.undo()

    def set_dz_max(self):
        dz_max = self.dz_max.text().replace(",", ".")
        if valid_float_input(dz_max):
            self.calc_settings["Maximum dz"] = float(dz_max)
            self.dz_max.setText(dz_max)
        else:
            self.dz_max.undo()

    def set_dlns_max(self):
        dlns_max = self.dlns_max.text().replace(",", ".")
        if valid_float_input(dlns_max):
            self.calc_settings["Maximum dlns"] = float(dlns_max)
            self.dlns_max.setText(dlns_max)
        else:
            self.dlns_max.undo()

    def set_line_color(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][0] = color

    def set_grid(self, is_checked):
        self.plotting_options["Grid on"] = is_checked

    def set_plot_title(self):
        title = self.title.text()
        self.plotting_options["Title"] = title

    def set_plot_x_label(self):
        xlabel = self.xlabel.text()
        self.plotting_options["x label"] = xlabel

    def set_plot_y_label(self):
        ylabel = self.ylabel.text()
        self.plotting_options["y label"] = ylabel

    def restore_defaults(self):
        calc_settings = self.default["Binary pxy"]["Calc"]
        plotting_options = self.default["Binary pxy"]["Plotting"]

        self.temp.setText(str(calc_settings["Temperature"]))
        self.p_max.setText(str(calc_settings["Maximum pressure"]))
        self.p_min.setText(str(calc_settings["Minimum pressure"]))
        self.dz_max.setText(str(calc_settings["Maximum dz"]))
        self.dlns_max.setText(str(calc_settings["Maximum dlns"]))

        self.set_temp()
        self.set_p_max()
        self.set_p_min()
        self.set_dz_max()
        self.set_dlns_max()

        self.set_line_color(plotting_options["Colors"][0])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText(plotting_options["Title"])
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])

        self.set_grid(self.grid_checkbox.isChecked())
        self.set_plot_title()
        self.set_plot_x_label()
        self.set_plot_y_label()


class PRhoOptionsWindow(QDialog):
    """
    A window containing the parameters for the pressure density plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("widgets/layouts/pressure_density_options.ui", self)
        self.setWindowTitle("Pressure density options")

        self.calc_settings = plotting_preferences["Pressure density"]["TPV"]
        self.crit_point_settings = plotting_preferences["Pressure density"]["Critical"]
        self.plotting_options = plotting_preferences["Pressure density"]["Plotting"]

        self.default = default

        # Set initial data

        self.p_0.setText(str(self.calc_settings["Initial pressure"]))
        self.p_max.setText(str(self.calc_settings["Maximum pressure"]))
        self.t_min.setText(str(self.calc_settings["Minimum temperature"]))
        self.step_size.setText(str(self.calc_settings["Step size"]))

        self.crit_t.setText(str(self.crit_point_settings["Temperature"]))
        self.crit_v.setText(str(self.crit_point_settings["Volume"]))
        self.crit_tol.setText(str(self.crit_point_settings["Error tolerance"]))

        self.set_line_color(self.plotting_options["Colors"][0])
        self.set_point_color(self.plotting_options["Colors"][1])
        self.grid_checkbox.setChecked(self.plotting_options["Grid on"])
        self.title.setText(self.plotting_options["Title"])
        self.xlabel.setText(self.plotting_options["x label"])
        self.ylabel.setText(self.plotting_options["y label"])

        self.setFocus()

        # Validators for input
        float_validator = QDoubleValidator()
        locale = QLocale(QLocale.English)
        float_validator.setLocale(locale)

        self.p_0.setValidator(float_validator)
        self.p_max.setValidator(float_validator)
        self.t_min.setValidator(float_validator)
        self.step_size.setValidator(float_validator)

        self.crit_t.setValidator(float_validator)
        self.crit_v.setValidator(float_validator)
        self.crit_tol.setValidator(float_validator)

        # Action handling

        self.p_0.editingFinished.connect(self.set_p_0)
        self.p_max.editingFinished.connect(self.set_p_max)
        self.t_min.editingFinished.connect(self.set_t_min)
        self.step_size.editingFinished.connect(self.set_step_size)

        self.crit_t.editingFinished.connect(self.set_crit_t)
        self.crit_v.editingFinished.connect(self.set_crit_v)
        self.crit_tol.editingFinished.connect(self.set_crit_tol)

        self.line_color_tool_btn.clicked.connect(self.set_line_color)
        self.point_color_tool_btn.clicked.connect(self.set_point_color)
        self.grid_checkbox.clicked.connect(self.set_grid)
        self.title.editingFinished.connect(self.set_plot_title)
        self.xlabel.editingFinished.connect(self.set_plot_x_label)
        self.ylabel.editingFinished.connect(self.set_plot_y_label)

        self.save_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_p_0(self):
        p_0 = self.p_0.text().replace(",", ".")
        if valid_float_input(p_0):
            self.calc_settings["Initial pressure"] = float(p_0)
            self.p_0.setText(p_0)
        else:
            self.p_0.undo()

    def set_p_max(self):
        p_max = self.p_max.text().replace(",", ".")
        if valid_float_input(p_max):
            self.calc_settings["Maximum pressure"] = float(p_max)
            self.p_max.setText(p_max)
        else:
            self.p_max.undo()

    def set_t_min(self):
        t_min = self.t_min.text().replace(",", ".")
        if valid_float_input(t_min):
            self.calc_settings["Minimum temperature"] = float(t_min)
            self.t_min.setText(t_min)
        else:
            self.t_min.undo()

    def set_step_size(self):
        step_size = self.step_size.text().replace(",", ".")
        if valid_float_input(step_size):
            self.calc_settings["Step size"] = float(step_size)
            self.step_size.setText(step_size)
        else:
            self.step_size.undo()

    def set_crit_t(self):
        crit_t = self.crit_t.text().replace(",", ".")
        if valid_float_input(crit_t):
            self.crit_point_settings["Temperature"] = float(crit_t)
            self.crit_t.setText(crit_t)
        else:
            self.crit_t.undo()

    def set_crit_v(self):
        crit_v = self.crit_v.text().replace(",", ".")
        if valid_float_input(crit_v):
            self.crit_point_settings["Volume"] = float(crit_v)
            self.crit_v.setText(crit_v)
        else:
            self.crit_v.undo()

    def set_crit_tol(self):
        crit_tol = self.crit_tol.text().replace(",", ".")
        if valid_float_input(crit_tol):
            self.crit_point_settings["Error tolerance"] = float(crit_tol)
            self.crit_tol.setText(crit_tol)
        else:
            self.crit_tol.undo()

    def set_line_color(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][0] = color

    def set_point_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.point_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.point_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][1] = color

    def set_isopleth_1_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_1_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_1_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][2] = color

    def set_isopleth_2_color(self, color=None):
        """
        Opens a color picker and sets point color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.isopleth_2_color_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.isopleth_2_color_preview.setStyleSheet(style)

        self.plotting_options["Colors"][3] = color

    def set_grid(self, is_checked):
        self.plotting_options["Grid on"] = is_checked

    def set_plot_title(self):
        title = self.title.text()
        self.plotting_options["Title"] = title

    def set_plot_x_label(self):
        xlabel = self.xlabel.text()
        self.plotting_options["x label"] = xlabel

    def set_plot_y_label(self):
        ylabel = self.ylabel.text()
        self.plotting_options["y label"] = ylabel

    def restore_defaults(self):
        calc_settings = self.default["Pressure density"]["TPV"]
        crit_point_settings = self.default["Pressure density"]["Critical"]
        plotting_options = self.default["Pressure density"]["Plotting"]

        self.p_0.setText(str(calc_settings["Initial pressure"]))
        self.p_max.setText(str(calc_settings["Maximum pressure"]))
        self.t_min.setText(str(calc_settings["Minimum temperature"]))
        self.step_size.setText(str(calc_settings["Step size"]))

        self.set_p_0()
        self.set_p_max()
        self.set_t_min()
        self.set_step_size()

        self.crit_t.setText(str(crit_point_settings["Temperature"]))
        self.crit_v.setText(str(crit_point_settings["Volume"]))
        self.crit_tol.setText(str(crit_point_settings["Error tolerance"]))

        self.set_crit_t()
        self.set_crit_v()
        self.set_crit_tol()

        self.set_line_color(plotting_options["Colors"][0])
        self.set_point_color(plotting_options["Colors"][1])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText(plotting_options["Title"])
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])

        self.set_grid(self.grid_checkbox.isChecked())
        self.set_plot_title()
        self.set_plot_x_label()
        self.set_plot_y_label()


class GlobalBinaryOptionsWindow(QDialog):
    """
    A window containing the parameters for the global binary plot
    """

    def __init__(self, plotting_preferences, default):
        QDialog.__init__(self)
        loadUi("widgets/layouts/global_binary_options.ui", self)
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
        float_validator = QDoubleValidator()
        locale = QLocale(QLocale.English)
        float_validator.setLocale(locale)

        self.p_min.setValidator(float_validator)
        self.t_min.setValidator(float_validator)

        # Action handling
        self.p_min.editingFinished.connect(self.set_p_min)
        self.t_min.editingFinished.connect(self.set_t_min)
        self.azeotropes_checkbox.clicked.connect(self.set_azeotropes)

        self.line_color_1_tool_btn.clicked.connect(self.set_line_color_1)
        self.line_color_2_tool_btn.clicked.connect(self.set_line_color_2)
        self.line_color_3_tool_btn.clicked.connect(self.set_line_color_3)
        self.line_color_4_tool_btn.clicked.connect(self.set_line_color_4)
        self.grid_checkbox.clicked.connect(self.set_grid)
        self.title.editingFinished.connect(self.set_plot_title)
        self.xlabel.editingFinished.connect(self.set_plot_x_label)
        self.ylabel.editingFinished.connect(self.set_plot_y_label)

        self.save_btn.clicked.connect(self.close)
        self.restore_defaults_btn.clicked.connect(self.restore_defaults)

    def set_p_min(self):
        p_min = self.p_min.text().replace(",", ".")
        if valid_float_input(p_min):
            self.calc_settings["Minimum pressure"] = float(p_min)
            self.p_min.setText(p_min)
        else:
            self.p_min.undo()

    def set_t_min(self):
        t_min = self.t_min.text().replace(",", ".")
        if valid_float_input(t_min):
            self.calc_settings["Minimum temperature"] = float(t_min)
            self.t_min.setText(t_min)
        else:
            self.t_min.undo()

    def set_azeotropes(self, is_checked):
        self.calc_settings["Azeotropes"] = is_checked

    def set_line_color_1(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_1_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_1_preview.setStyleSheet(style)

        self.plotting_options["Colors"][0] = color

    def set_line_color_2(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_2_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_2_preview.setStyleSheet(style)

        self.plotting_options["Colors"][1] = color

    def set_line_color_3(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_3_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_3_preview.setStyleSheet(style)

        self.plotting_options["Colors"][2] = color

    def set_line_color_4(self, color=None):
        """
        Opens a color picker and sets line color for the plot if a color is not specified
        """
        if not color:
            initial_color = self.line_color_4_preview.palette().window().color()
            color = QColorDialog.getColor(initial_color).name()

        style = "background-color: %s; border-radius: 2px; border: 1px solid #124d77;" % color
        self.line_color_4_preview.setStyleSheet(style)

        self.plotting_options["Colors"][3] = color

    def set_grid(self, is_checked):
        self.plotting_options["Grid on"] = is_checked

    def set_plot_title(self):
        title = self.title.text()
        self.plotting_options["Title"] = title

    def set_plot_x_label(self):
        xlabel = self.xlabel.text()
        self.plotting_options["x label"] = xlabel

    def set_plot_y_label(self):
        ylabel = self.ylabel.text()
        self.plotting_options["y label"] = ylabel

    def restore_defaults(self):
        calc_settings = self.default["Global binary"]["Calc"]
        plotting_options = self.default["Global binary"]["Plotting"]

        self.p_min.setText(str(calc_settings["Minimum pressure"]))
        self.t_min.setText(str(calc_settings["Minimum temperature"]))
        self.azeotropes_checkbox.setChecked(calc_settings["Azeotropes"])

        self.set_p_min()
        self.set_t_min()
        self.set_azeotropes(self.azeotropes_checkbox.isChecked())

        self.set_line_color_1(plotting_options["Colors"][0])
        self.set_line_color_2(plotting_options["Colros"][1])
        self.set_line_color_3(plotting_options["Colors"][2])
        self.set_line_color_4(plotting_options["Colors"][3])
        self.grid_checkbox.setChecked(plotting_options["Grid on"])
        self.title.setText("van Konyenburg and Scott type: ")
        self.xlabel.setText(plotting_options["x label"])
        self.ylabel.setText(plotting_options["y label"])

        self.set_grid(self.grid_checkbox.isChecked())
        self.set_plot_title()
        self.set_plot_x_label()
        self.set_plot_y_label()
