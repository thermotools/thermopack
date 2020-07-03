from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
from matplotlib.figure import Figure

import numpy as np

N_ISOPLETHS = 15
NMAX = 50


class MplCanvas(FigureCanvasQTAgg):
    def __init__(self, components):
        self.fig = Figure(dpi=100)
        self.empty = True
        self.components = components
        self.isenthalps = None
        self.isentropes = None
        self.isotherms = None
        self.isobars = None
        super(MplCanvas, self).__init__(figure=self.fig)

    def toggle_isenthalps(self, is_checked):
        if not self.empty and self.isenthalps:
            if is_checked:
                for line in self.isenthalps:
                    line.set_linestyle("solid")
            else:
                for line in self.isenthalps:
                    line.set_linestyle("None")
            self.draw()

    def toggle_isentropes(self, is_checked):
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

    def plot_envelope(self, tp, prim_vars, fractions, line_color, point_color, grid_on):
        p_initial = 1.0e5
        p_max = 1.5e7
        T_min = None
        step_size = 0.1

        # Calculate T, P, V
        T, P, V = tp.get_envelope_twophase(initial_pressure=p_initial, z=fractions, maximum_pressure=p_max,
                                           minimum_temperature=T_min, step_size=step_size, calc_v=True)

        H = np.array([tp.enthalpy_tv(T[i], V[i], fractions) for i in range(len(T))])
        S = np.array([tp.entropy_tv(T[i], V[i], fractions) for i in range(len(T))])

        global H_list_test
        global T_list_test
        global S_list_test
        global P_list_test

        H_list_test = np.linspace(np.min(H), np.max(H), N_ISOPLETHS)
        S_list_test = np.linspace(np.min(S), np.max(S), N_ISOPLETHS)
        T_list_test = np.linspace(np.min(T) * 0.60, np.max(T) * 1.40, N_ISOPLETHS)
        P_list_test = np.linspace(np.min(P) * 0.60, np.max(P) * 1.40, N_ISOPLETHS)

        # Calculate critical variables
        T_c, V_c, P_c = tp.critical(n=fractions, temp=0.0, v=0.0, tol=1.0e-7)

        H_c = tp.enthalpy_tv(T_c, V_c, fractions)
        S_c = tp.entropy_tv(T_c, V_c, fractions)

        # Plot depending on which primary variables are chosen
        if prim_vars == "PT":
            x, y, crit_x, crit_y, xlabel, ylabel = self.plot_envelope_PT(tp, T, P, T_c, P_c, fractions)

        elif prim_vars == "PH":
            x, y, crit_x, crit_y, xlabel, ylabel = self.plot_envelope_PH(tp, P, H, P_c, H_c, fractions)

        elif prim_vars == "PS":
            x, y, crit_x, crit_y, xlabel, ylabel = self.plot_envelope_PS(tp, P, S, P_c, S_c, fractions)

        elif prim_vars == "TH":
            x, y, crit_x, crit_y, xlabel, ylabel = self.plot_envelope_TH(tp, T, H, T_c, H_c, fractions)

        elif prim_vars == "TS":
            x, y, crit_x, crit_y, xlabel, ylabel = self.plot_envelope_TS(tp, T, S, T_c, S_c, fractions)

        else:
            return

        # Plotting

        self.axes.plot(x, y, color=line_color, label="Phase envelope")
        self.axes.scatter([crit_x], [crit_y], color=point_color, label="Critical point")

        self.axes.grid(grid_on)
        self.axes.set_xlabel(xlabel)
        self.axes.set_ylabel(ylabel)

        # Sort entries in the legend
        handles, labels = self.axes.get_legend_handles_labels()
        self.axes.legend([handles[3], handles[2], handles[0], handles[1]],
                         [labels[3], labels[2], labels[0], labels[1]],
                         loc="best")
        self.draw()

    def plot_envelope_PT(self, tp, T, P, T_c, P_c, fractions):

        # Display correct buttons
        self.parent().parent().parent().btn_stack.setCurrentIndex(0)
        self.parent().parent().parent().PT_H_btn.setChecked(True)
        self.parent().parent().parent().PT_S_btn.setChecked(True)

        x = T
        y = P

        crit_x = T_c
        crit_y = P_c

        # Isenthalps, isentropes
        enthalpies = H_list_test
        entropies = S_list_test

        self.isenthalps = []
        self.isentropes = []

        P_min = 1.0e5
        P_max = 1.5e7
        T_min = 200.0
        T_max = 500.0
        nmax = NMAX

        for i in range(len(enthalpies)):
            t_vals, p_vals, v_vals, s_vals = tp.get_isenthalp(enthalpies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                h_line, = self.axes.plot(t_vals, p_vals, color="#ffd2d2", label="Isenthalp")
            else:
                h_line, = self.axes.plot(t_vals, p_vals, color="#ffd2d2")

            self.isenthalps.append(h_line)

            t_vals, p_vals, v_vals, h_vals = tp.get_isentrope(entropies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                s_line, = self.axes.plot(t_vals, p_vals, color="#d5d3ff", label="Isentrope")
            else:
                s_line, = self.axes.plot(t_vals, p_vals, color="#d5d3ff")

            self.isentropes.append(s_line)

        self.isotherms = None
        self.isobars = None

        xlabel = "T [K]"
        ylabel = "P [Pa]"

        return x, y, crit_x, crit_y, xlabel, ylabel

    def plot_envelope_PH(self, tp, P, H, P_c, H_c, fractions):

        # Display correct buttons
        self.parent().parent().parent().btn_stack.setCurrentIndex(1)
        self.parent().parent().parent().PH_T_btn.setChecked(True)
        self.parent().parent().parent().PH_S_btn.setChecked(True)

        x = H
        y = P

        crit_x = H_c
        crit_y = P_c

        # isotherms, isentropes
        temperatures = T_list_test
        entropies = S_list_test

        self.isotherms = []
        self.isentropes = []

        P_min = 1.0e5
        P_max = 1.5e7
        T_min = 200.0
        T_max = 500.0
        nmax = NMAX

        for i in range(len(temperatures)):
            p_vals, v_vals, s_vals, h_vals = tp.get_isotherm(temperatures[i], fractions, minimum_pressure=P_min,
                                                             maximum_pressure=P_max, nmax=nmax)

            if i == 0:
                t_line, = self.axes.plot(h_vals, p_vals, color="#ffd2d2", label="Isotherm")
            else:
                t_line, = self.axes.plot(h_vals, p_vals, color="#ffd2d2")

            self.isotherms.append(t_line)

            t_vals, p_vals, v_vals, h_vals = tp.get_isentrope(entropies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                s_line, = self.axes.plot(h_vals, p_vals, color="#d5d3ff", label="Isentrope")
            else:
                s_line, = self.axes.plot(h_vals, p_vals, color="#d5d3ff")

            self.isentropes.append(s_line)

        self.isenthalps = None
        self.isobars = None

        xlabel = "H [J / mol]"
        ylabel = "P [Pa]"

        return x, y, crit_x, crit_y, xlabel, ylabel

    def plot_envelope_PS(self, tp, P, S, P_c, S_c, fractions):

        # Display correct buttons
        self.parent().parent().parent().btn_stack.setCurrentIndex(2)
        self.parent().parent().parent().PS_T_btn.setChecked(True)
        self.parent().parent().parent().PS_H_btn.setChecked(True)

        x = S
        y = P

        crit_x = S_c
        crit_y = P_c

        # isotherms, isenthalps
        temperatures = T_list_test
        enthalpies = H_list_test

        self.isotherms = []
        self.isenthalps = []

        P_min = 1.0e5
        P_max = 1.5e7
        T_min = 200.0
        T_max = 500.0
        nmax = NMAX

        for i in range(len(temperatures)):
            p_vals, v_vals, s_vals, h_vals = tp.get_isotherm(temperatures[i], fractions, minimum_pressure=P_min,
                                                             maximum_pressure=P_max, nmax=nmax)
            if i == 0:
                t_line, = self.axes.plot(s_vals, p_vals, color="#ffd2d2", label="Isotherm")
            else:
                t_line, = self.axes.plot(s_vals, p_vals, color="#ffd2d2")

            self.isotherms.append(t_line)

            t_vals, p_vals, v_vals, s_vals = tp.get_isenthalp(enthalpies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)

            if i == 0:
                h_line, = self.axes.plot(s_vals, p_vals, color="#d5d3ff", label="Isenthalp")
            else:
                h_line, = self.axes.plot(s_vals, p_vals, color="#d5d3ff")

            self.isenthalps.append(h_line)

        self.isentropes = None
        self.isobars = None

        xlabel = "S [J / K mol]"
        ylabel = "P [Pa]"

        return x, y, crit_x, crit_y, xlabel, ylabel

    def plot_envelope_TH(self, tp, T, H, T_c, H_c, fractions):

        # Display correct buttons
        self.parent().parent().parent().btn_stack.setCurrentIndex(3)
        self.parent().parent().parent().TH_S_btn.setChecked(True)
        self.parent().parent().parent().TH_P_btn.setChecked(True)

        x = H
        y = T

        crit_x = H_c
        crit_y = T_c

        # isobars, isentropes
        pressures = P_list_test
        entropies = S_list_test

        self.isobars = []
        self.isentropes = []

        P_min = 1.0e5
        P_max = 1.5e7
        T_min = 200.0
        T_max = 500.0
        nmax = NMAX

        for i in range(len(pressures)):
            t_vals, v_vals, s_vals, h_vals = tp.get_isobar(pressures[i], fractions, minimum_temperature=T_min,
                                                           maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                p_line, = self.axes.plot(h_vals, t_vals, color="#ffd2d2", label="Isobar")
            else:
                p_line, = self.axes.plot(h_vals, t_vals, color="#ffd2d2")

            self.isobars.append(p_line)

            t_vals, p_vals, v_vals, h_vals = tp.get_isentrope(entropies[i], fractions, minimum_pressure=P_min,
                                                              maximum_pressure=P_max, minimum_temperature=T_min,
                                                              maximum_temperature=T_max, nmax=nmax)
            if i == 0:
                s_line, = self.axes.plot(h_vals, t_vals, color="#d5d3ff", label="Isentrope")
            else:
                s_line, = self.axes.plot(h_vals, t_vals, color="#d5d3ff")

            self.isentropes.append(s_line)

        self.isenthalps = None
        self.isotherms = None

        xlabel = "H [J / mol]"
        ylabel = "T [K]"

        return x, y, crit_x, crit_y, xlabel, ylabel

    def plot_envelope_TS(self, tp, T, S, T_c, S_c, fractions):

        # Display correct buttons
        self.parent().parent().parent().btn_stack.setCurrentIndex(4)
        self.parent().parent().parent().TS_P_btn.setChecked(True)
        self.parent().parent().parent().TS_H_btn.setChecked(True)

        x = S
        y = T

        crit_x = S_c
        crit_y = T_c

        # Isenthalps, isobars
        pressures = P_list_test
        enthalpies = H_list_test

        self.isenthalps = []
        self.isobars = []

        P_min = 1.0e5
        P_max = 1.5e7
        T_min = 200.0
        T_max = 500.0
        nmax = NMAX

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

        xlabel = "S [J / K mol]"
        ylabel = "T [K]"

        return x, y, crit_x, crit_y, xlabel, ylabel

    def plot_binary_pxy(self, tp, line_color, grid_on):

        T = 288.0
        p_max = 1.5e7
        p_min = 1.0e5
        dz_max = 0.003
        dlns_max = 0.01

        LLE, L1VE, L2VE = tp.get_binary_pxy(temp=T, maximum_pressure=p_max, minimum_pressure=p_min,
                                            maximum_dz=dz_max, maximum_dlns=dlns_max)

        if LLE[0] is not None:
            self.axes.plot(LLE[0], LLE[2], color=line_color)
            self.axes.plot(LLE[1], LLE[2], color=line_color)

        if L1VE[0] is not None:
            self.axes.plot(L1VE[0], L1VE[2], color=line_color)
            self.axes.plot(L1VE[1], L1VE[2], color=line_color)

        if L2VE[0] is not None:
            self.axes.plot(L2VE[0], L2VE[2], color=line_color)
            self.axes.plot(L2VE[1], L2VE[2], color=line_color)

        self.axes.grid(grid_on)
        self.axes.set_xlabel("xlabel")
        self.axes.set_ylabel("ylabel")
        self.draw()
        self.draw()
