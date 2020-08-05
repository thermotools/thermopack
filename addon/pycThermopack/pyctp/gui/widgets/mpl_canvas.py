from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure

import numpy as np


class MplCanvas(FigureCanvasQTAgg):
    """
    A canvas for matplotlib plots. Contains all plot functionality for Plot Mode
    """

    def __init__(self, components, plotting_preferences):
        self.fig = Figure(dpi=100)
        self.empty = True
        self.components = components
        self.isenthalps = None
        self.isentropes = None
        self.isotherms = None
        self.isobars = None
        super(MplCanvas, self).__init__(figure=self.fig)

        self.plotting_preferences = plotting_preferences

    def toggle_isenthalps(self, is_checked):
        """
        Hides / shows isenthalp lines in the plot if a plot exists
        :param is_checked: Status of isenthalp button (bool)
        """
        if not self.empty and self.isenthalps:
            if is_checked:
                for line in self.isenthalps:
                    line.set_linestyle("solid")
            else:
                for line in self.isenthalps:
                    line.set_linestyle("None")
            self.draw()

    def toggle_isentropes(self, is_checked):
        """
        Hides / shows isentrope lines in the plot if a plot exists
        :param is_checked: Status of isentrope button (bool)
        """
        if not self.empty and self.isentropes:
            if is_checked:
                for line in self.isentropes:
                    line.set_linestyle("solid")
            else:
                for line in self.isentropes:
                    line.set_linestyle("None")
            self.draw()
        else:
            return

    def toggle_isotherms(self, is_checked):
        """
        Hides / shows isotherm lines in the plot if a plot exists
        :param is_checked: Status of isotherm button (bool)
        """
        if not self.empty and self.isotherms:
            if is_checked:
                for line in self.isotherms:
                    line.set_linestyle("solid")
            else:
                for line in self.isotherms:
                    line.set_linestyle("None")
            self.draw()
        else:
            return

    def toggle_isobars(self, is_checked):
        """
        Hides / shows isobar lines in the plot if a plot exists
        :param is_checked: Status of isobar button (bool)
        """
        if not self.empty and self.isobars:
            if is_checked:
                for line in self.isobars:
                    line.set_linestyle("solid")
            else:
                for line in self.isobars:
                    line.set_linestyle("None")
            self.draw()
        else:
            return

    def plot_envelope(self, tp, prim_vars, fractions):
        """
        Plots a phase envelope
        :param tp: Thermopack instance
        :param prim_vars: Primary variables for the plot (e.g. PT, PH, ..)
        :param fractions: List of molar fractions for the components
        """

        tpv_settings = self.plotting_preferences["Phase envelope"]["TPV"]
        isopleth_settings = self.plotting_preferences["Phase envelope"]["Isopleths"]
        critical_settings = self.plotting_preferences["Phase envelope"]["Critical"]
        plot_settings = self.plotting_preferences["Phase envelope"]["Plotting"]

        p_initial = tpv_settings["Initial pressure"]
        t_min = tpv_settings["Minimum temperature"]
        p_max = tpv_settings["Maximum pressure"]
        step_size = tpv_settings["Step size"]

        # Calculate T, P, V
        T, P, V = tp.get_envelope_twophase(initial_pressure=p_initial, z=fractions, maximum_pressure=p_max,
                                           minimum_temperature=t_min, step_size=step_size, calc_v=True)

        H = np.array([tp.enthalpy_tv(T[i], V[i], fractions) for i in range(len(T))])
        S = np.array([tp.entropy_tv(T[i], V[i], fractions) for i in range(len(T))])

        global H_list
        global T_list
        global S_list
        global P_list

        n_isopleths = isopleth_settings["Number of isopleths"]

        H_list = np.linspace(np.min(H), np.max(H), n_isopleths)
        S_list = np.linspace(np.min(S), np.max(S), n_isopleths)
        T_list = np.linspace(np.min(T) * 0.60, np.max(T) * 1.40, n_isopleths)
        P_list = np.linspace(np.min(P) * 0.60, np.max(P) * 1.40, n_isopleths)

        temp = critical_settings["Temperature"]
        v = critical_settings["Volume"]
        tol = critical_settings["Error tolerance"]

        # Calculate critical variables
        try:
            T_c, V_c, P_c = tp.critical(n=fractions, temp=temp, v=v, tol=tol)
            H_c = tp.enthalpy_tv(T_c, V_c, fractions)
            S_c = tp.entropy_tv(T_c, V_c, fractions)
        except Exception:
            T_c, V_c, P_c, H_c, S_c = None, None, None, None, None

        # Set global variables, so that they are accessible in all phase envelope plot functions
        global isopleth_1_color
        global isopleth_2_color
        global P_min
        global P_max
        global T_min
        global T_max
        global nmax

        isopleth_1_color = plot_settings["Colors"][2]
        isopleth_2_color = plot_settings["Colors"][3]
        P_min = isopleth_settings["Minimum pressure"]
        P_max = isopleth_settings["Maximum pressure"]
        T_min = isopleth_settings["Minimum temperature"]
        T_max = isopleth_settings["Maximum temperature"]
        nmax = isopleth_settings["N max"]

        # Plot depending on which primary variables are chosen
        if prim_vars == "PT":
            x, y, crit_x, crit_y = self.plot_envelope_PT(tp, T, P, T_c, P_c, fractions)

        elif prim_vars == "PH":
            x, y, crit_x, crit_y = self.plot_envelope_PH(tp, P, H, P_c, H_c, fractions)

        elif prim_vars == "PS":
            x, y, crit_x, crit_y = self.plot_envelope_PS(tp, P, S, P_c, S_c, fractions)

        elif prim_vars == "TH":
            x, y, crit_x, crit_y = self.plot_envelope_TH(tp, T, H, T_c, H_c, fractions)

        elif prim_vars == "TS":
            x, y, crit_x, crit_y = self.plot_envelope_TS(tp, T, S, T_c, S_c, fractions)

        else:
            return

        # Plotting

        line_color = plot_settings["Colors"][0]
        point_color = plot_settings["Colors"][1]
        grid_on = plot_settings["Grid on"]
        xlabel = plot_settings["x label"]
        ylabel = plot_settings["y label"]
        title = plot_settings["Title"]

        self.axes.plot(x, y, color=line_color, label="Phase envelope")
        self.axes.scatter([crit_x], [crit_y], color=point_color, label="Critical point")

        self.axes.set_title(title)
        self.axes.grid(grid_on)
        self.axes.set_xlabel(xlabel)
        self.axes.set_ylabel(ylabel)

        # Sort entries in the legend
        if n_isopleths > 0:
            handles, labels = self.axes.get_legend_handles_labels()
            self.axes.legend([handles[3], handles[2], handles[0], handles[1]],
                             [labels[3], labels[2], labels[0], labels[1]],
                             loc="best")
        self.draw()

    def plot_envelope_PT(self, tp, T, P, T_c, P_c, fractions):
        """
        Return plot data for a PT phase envelope
        :param tp: Thermopack instance
        :param T: Temperature values
        :param P: Pressure values
        :param T_c: Critical temperature
        :param P_c: Critical pressure
        :param fractions: List of molar fractions
        :return:    x: x values for plot,
                    y: y values for plot,
                    crit_x: x value for critical point,
                    crit_y: y value for critical point,
        """

        # Display correct buttons
        self.parent().parent().parent().isopleth_btn_stack.setCurrentIndex(0)
        self.parent().parent().parent().PT_H_btn.setChecked(True)
        self.parent().parent().parent().PT_S_btn.setChecked(True)

        x = T
        y = P

        crit_x = T_c
        crit_y = P_c

        # Isenthalps, isentropes
        enthalpies = H_list
        entropies = S_list

        self.isenthalps = []
        self.isentropes = []

        for i in range(len(enthalpies)):
            t_vals, p_vals, v_vals, s_vals = tp.get_isenthalp(enthalpies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                h_line, = self.axes.plot(t_vals, p_vals, color=isopleth_1_color, label="Isenthalp")
            else:
                h_line, = self.axes.plot(t_vals, p_vals, color=isopleth_1_color)

            self.isenthalps.append(h_line)

            t_vals, p_vals, v_vals, h_vals = tp.get_isentrope(entropies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                s_line, = self.axes.plot(t_vals, p_vals, color=isopleth_2_color, label="Isentrope")
            else:
                s_line, = self.axes.plot(t_vals, p_vals, color=isopleth_2_color)

            self.isentropes.append(s_line)

        self.isotherms = None
        self.isobars = None

        return x, y, crit_x, crit_y

    def plot_envelope_PH(self, tp, P, H, P_c, H_c, fractions):
        """
        Return plot data for a PH phase envelope
        :param tp: Thermopack instance
        :param P: Pressure values
        :param H: Enthalpy values
        :param P_c: Critical pressure
        :param H_c: Critical enthalpy
        :param fractions: List of molar fractions
        :return:    x: x values for plot,
                    y: y values for plot,
                    crit_x: x value for critical point,
                    crit_y: y value for critical point,
        """

        # Display correct buttons
        self.parent().parent().parent().isopleth_btn_stack.setCurrentIndex(1)
        self.parent().parent().parent().PH_T_btn.setChecked(True)
        self.parent().parent().parent().PH_S_btn.setChecked(True)

        x = H
        y = P

        crit_x = H_c
        crit_y = P_c

        # isotherms, isentropes
        temperatures = T_list
        entropies = S_list

        self.isotherms = []
        self.isentropes = []

        for i in range(len(temperatures)):
            p_vals, v_vals, s_vals, h_vals = tp.get_isotherm(temperatures[i], fractions, minimum_pressure=P_min,
                                                             maximum_pressure=P_max, nmax=nmax)

            if i == 0:
                t_line, = self.axes.plot(h_vals, p_vals, color=isopleth_1_color, label="Isotherm")
            else:
                t_line, = self.axes.plot(h_vals, p_vals, color=isopleth_1_color)

            self.isotherms.append(t_line)

            t_vals, p_vals, v_vals, h_vals = tp.get_isentrope(entropies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                s_line, = self.axes.plot(h_vals, p_vals, color=isopleth_2_color, label="Isentrope")
            else:
                s_line, = self.axes.plot(h_vals, p_vals, color=isopleth_2_color)

            self.isentropes.append(s_line)

        self.isenthalps = None
        self.isobars = None

        return x, y, crit_x, crit_y

    def plot_envelope_PS(self, tp, P, S, P_c, S_c, fractions):
        """
        Return plot data for a PS phase envelope
        :param tp: Thermopack instance
        :param P: Pressure values
        :param S: Entropy values
        :param P_c: Critical pressure
        :param S_c: Critical entropy
        :param fractions: List of molar fractions
        :return:    x: x values for plot,
                    y: y values for plot,
                    crit_x: x value for critical point,
                    crit_y: y value for critical point,
        """

        # Display correct buttons
        self.parent().parent().parent().isopleth_btn_stack.setCurrentIndex(2)
        self.parent().parent().parent().PS_T_btn.setChecked(True)
        self.parent().parent().parent().PS_H_btn.setChecked(True)

        x = S
        y = P

        crit_x = S_c
        crit_y = P_c

        # isotherms, isenthalps
        temperatures = T_list
        enthalpies = H_list

        self.isotherms = []
        self.isenthalps = []

        for i in range(len(temperatures)):
            p_vals, v_vals, s_vals, h_vals = tp.get_isotherm(temperatures[i], fractions, minimum_pressure=P_min,
                                                             maximum_pressure=P_max, nmax=nmax)
            if i == 0:
                t_line, = self.axes.plot(s_vals, p_vals, color=isopleth_1_color, label="Isotherm")
            else:
                t_line, = self.axes.plot(s_vals, p_vals, color=isopleth_1_color)

            self.isotherms.append(t_line)

            t_vals, p_vals, v_vals, s_vals = tp.get_isenthalp(enthalpies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)

            if i == 0:
                h_line, = self.axes.plot(s_vals, p_vals, color=isopleth_2_color, label="Isenthalp")
            else:
                h_line, = self.axes.plot(s_vals, p_vals, color=isopleth_2_color)

            self.isenthalps.append(h_line)

        self.isentropes = None
        self.isobars = None

        return x, y, crit_x, crit_y

    def plot_envelope_TH(self, tp, T, H, T_c, H_c, fractions):
        """
        Return plot data for a PS phase envelope
        :param tp: Thermopack instance
        :param T: Temperature values
        :param H: Enthalpy values
        :param T_c: Critical temperature
        :param H_c: Critical enthalpy
        :param fractions: List of molar fractions
        :return:    x: x values for plot,
                    y: y values for plot,
                    crit_x: x value for critical point,
                    crit_y: y value for critical point,
        """

        # Display correct buttons
        self.parent().parent().parent().isopleth_btn_stack.setCurrentIndex(3)
        self.parent().parent().parent().TH_S_btn.setChecked(True)
        self.parent().parent().parent().TH_P_btn.setChecked(True)

        x = H
        y = T

        crit_x = H_c
        crit_y = T_c

        # isobars, isentropes
        pressures = P_list
        entropies = S_list

        self.isobars = []
        self.isentropes = []

        for i in range(len(pressures)):
            t_vals, v_vals, s_vals, h_vals = tp.get_isobar(pressures[i], fractions, minimum_temperature=200.0,
                                                           maximum_temperature=500.0, nmax=100)
            if i == 0:
                p_line, = self.axes.plot(h_vals, t_vals, color=isopleth_1_color, label="Isobar")
            else:
                p_line, = self.axes.plot(h_vals, t_vals, color=isopleth_1_color)

            self.isobars.append(p_line)

            t_vals, p_vals, v_vals, h_vals = tp.get_isentrope(entropies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                s_line, = self.axes.plot(h_vals, t_vals, color=isopleth_2_color, label="Isentrope")
            else:
                s_line, = self.axes.plot(h_vals, t_vals, color=isopleth_2_color)

            self.isentropes.append(s_line)

        self.isenthalps = None
        self.isotherms = None

        return x, y, crit_x, crit_y

    def plot_envelope_TS(self, tp, T, S, T_c, S_c, fractions):
        """
        Return plot data for a PS phase envelope
        :param tp: Thermopack instance
        :param T: Temperature values
        :param S: Entropy values
        :param T_c: Critical temperature
        :param S_c: Critical entropy
        :param fractions: List of molar fractions
        :return:    x: x values for plot,
                    y: y values for plot,
                    crit_x: x value for critical point,
                    crit_y: y value for critical point,
        """
        # Display correct buttons
        self.parent().parent().parent().isopleth_btn_stack.setCurrentIndex(4)
        self.parent().parent().parent().TS_P_btn.setChecked(True)
        self.parent().parent().parent().TS_H_btn.setChecked(True)

        x = S
        y = T

        crit_x = S_c
        crit_y = T_c

        # Isenthalps, isobars
        pressures = P_list
        enthalpies = H_list

        self.isenthalps = []
        self.isobars = []

        for i in range(len(pressures)):
            t_vals, v_vals, s_vals, h_vals = tp.get_isobar(pressures[i], fractions, minimum_temperature=T_min,
                                                           maximum_temperature=T_max)
            if i == 0:
                p_line, = self.axes.plot(s_vals, t_vals, color="#ffd2d2", label="Isobar")
            else:
                p_line, = self.axes.plot(s_vals, t_vals, color="#ffd2d2")

            self.isobars.append(p_line)

            t_vals, p_vals, v_vals, s_vals = tp.get_isenthalp(enthalpies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)

            if i == 0:
                h_line, = self.axes.plot(s_vals, p_vals, color="#d5d3ff", label="Isenthalp")
            else:
                h_line, = self.axes.plot(s_vals, p_vals, color="#d5d3ff")

            self.isenthalps.append(h_line)

        self.isentropes = None
        self.isotherms = None

        return x, y, crit_x, crit_y

    def plot_binary_pxy(self, tp):
        """
        Plots a binary pxy plot
        :param tp: Thermopack instance
        """

        calc_settings = self.plotting_preferences["Binary pxy"]["Calc"]
        plot_settings = self.plotting_preferences["Binary pxy"]["Plotting"]

        T = calc_settings["Temperature"]
        p_max = calc_settings["Maximum pressure"]
        p_min = calc_settings["Minimum pressure"]
        dz_max = calc_settings["Maximum dz"]
        dlns_max = calc_settings["Maximum dlns"]

        LLE, L1VE, L2VE = tp.get_binary_pxy(temp=T, maximum_pressure=p_max, minimum_pressure=p_min,
                                            maximum_dz=dz_max, maximum_dlns=dlns_max)

        line_color = plot_settings["Colors"][0]

        if LLE[0] is not None:
            self.axes.plot(LLE[0], LLE[2], color=line_color)
            self.axes.plot(LLE[1], LLE[2], color=line_color)

        if L1VE[0] is not None:
            self.axes.plot(L1VE[0], L1VE[2], color=line_color)
            self.axes.plot(L1VE[1], L1VE[2], color=line_color)

        if L2VE[0] is not None:
            self.axes.plot(L2VE[0], L2VE[2], color=line_color)
            self.axes.plot(L2VE[1], L2VE[2], color=line_color)

        grid_on = plot_settings["Grid on"]
        title = plot_settings["Title"]
        xlabel = plot_settings["x label"]
        ylabel = plot_settings["y label"]

        self.axes.grid(grid_on)
        self.axes.set_title(title)
        self.axes.set_xlabel(xlabel)
        self.axes.set_ylabel(ylabel)
        self.draw()

    def plot_pressure_density(self, tp, fractions):
        """
        Plots a pressure density plot
        :param tp: Thermopack instance
        :param fractions: List of molar fractions
        """

        calc_settings = self.plotting_preferences["Pressure density"]["Calc"]
        tpv_settings = self.plotting_preferences["Pressure density"]["TPV"]
        crit_settings = self.plotting_preferences["Pressure density"]["Critical"]
        plot_settings = self.plotting_preferences["Pressure density"]["Plotting"]

        p_initial = tpv_settings["Initial pressure"]
        t_min = tpv_settings["Minimum temperature"]
        p_max = tpv_settings["Maximum pressure"]
        step_size = tpv_settings["Step size"]

        # Calculate T, P, V
        T_ph_env, P_ph_env, V_ph_env = tp.get_envelope_twophase(initial_pressure=p_initial, z=fractions,
                                                                maximum_pressure=p_max,
                                                                minimum_temperature=t_min, step_size=step_size,
                                                                calc_v=True)

        crit_t_guess = crit_settings["Temperature"]
        crit_v_guess = crit_settings["Volume"]
        crit_tol = crit_settings["Error tolerance"]

        # Calculate critical T, V, P
        T_c, V_c, P_c = tp.critical(n=fractions, temp=crit_t_guess, v=crit_v_guess, tol=crit_tol)

        T_list = calc_settings["Temperatures"]
        V_start = V_c * calc_settings["Volume range start"]
        V_end = V_c * calc_settings["Volume range end"]
        V_num_points = calc_settings["Num points"]

        V_list = np.linspace(V_start, V_end, V_num_points)

        P_lists = []
        for T in T_list:
            P_list = []
            for V in V_list:
                P, = tp.pressure_tv(temp=T, volume=V, n=fractions)
                P_list.append(P)
            P_lists.append(P_list)

        rho_list = 1 / V_list

        title = plot_settings["Title"]
        grid_on = plot_settings["Grid on"]
        xlabel = plot_settings["x label"]
        ylabel = plot_settings["y label"]

        self.axes.plot([1 / v for v in V_ph_env], P_ph_env, label="Phase envelope")
        self.axes.scatter([1 / V_c], [P_c], label="Critical point")

        for i in range(len(P_lists)):
            self.axes.plot(rho_list, P_lists[i], label=str(T_list[i]) + " K")

        self.axes.set_title(title)
        self.axes.grid(grid_on)
        self.axes.set_xlabel(xlabel)
        self.axes.set_ylabel(ylabel)

        self.axes.legend(loc="best")
        self.draw()

    def plot_global_binary(self, tp):
        """
        Plots a binary pxy plot
        :param tp: Thermopack instance
        """

        calc_settings = self.plotting_preferences["Global binary"]["Calc"]
        plot_settings = self.plotting_preferences["Global binary"]["Plotting"]

        min_press = calc_settings["Minimum pressure"]
        min_temp = calc_settings["Minimum temperature"]
        azeotropes = calc_settings["Azeotropes"]

        KSTYPE, VLE, LLVE, CRIT, AZ = tp.global_binary_plot(minimum_pressure=min_press, minimum_temperature=min_temp,
                                                            include_azeotropes=azeotropes)

        colors = plot_settings["Colors"]
        linestyles = ["-", "--", ":", "-."]

        label = "VLE"
        for i in range(len(VLE)):
            self.axes.plot(VLE[i][:, 0], VLE[i][:, 1], linestyle=linestyles[0], color=colors[0], label=label)
            label = None

        label = "LLVE"
        for i in range(len(LLVE)):
            self.axes.plot(LLVE[i][:, 0], LLVE[i][:, 1], linestyle=linestyles[1], color=colors[1], label=label)
            label = None

        label = "CRIT"
        for i in range(len(CRIT)):
            self.axes.plot(CRIT[i][:, 0], CRIT[i][:, 1], linestyle=linestyles[2], color=colors[2], label=label)
            label = None

        label = "AZ"
        for i in range(len(AZ)):
            self.axes.plot(AZ[i][:, 0], AZ[i][:, 1], linestyle=linestyles[3], color=colors[3], label=label)
            label = None

        ks_strings = {
            1: "I",
            2: "II",
            3: "III",
            4: "IV",
            5: "V"
        }

        title = plot_settings["Title"]
        xlabel = plot_settings["x label"]
        ylabel = plot_settings["y label"]
        grid_on = plot_settings["Grid on"]

        if title == "van Konyenburg and Scott type: ":
            title += ks_strings[KSTYPE]

        self.axes.set_title(title)
        legend = self.axes.legend(loc="best", numpoints=1)
        legend.get_frame().set_linewidth(0.0)

        self.axes.set_xlabel(xlabel)
        self.axes.set_ylabel(ylabel)
        self.axes.grid(grid_on)
        self.draw()
