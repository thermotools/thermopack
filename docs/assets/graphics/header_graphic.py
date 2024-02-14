"""
This script was used to generate the header graphic (the nice flowy lines) it contains:

ColorGradient: A custom colormapping taking at least two colors to create a gradient effect
Colomap2D: A custom colormap class that takes several ColorGradients to create a 2d colormap

f, g, h : Functions that create fancy waves

Some plotting at the bottom to generate header.pdf

Feel free to play around with nice colors :)
Tip: Because a huge number of lines are used to create the gradient effects, this runs quite slowly. Reduce the resolution
in `xl` and the `nz` lists to get reasonable runtime while playing around.
"""
import numpy as np
from numpy import sin, cos
import matplotlib.pyplot as plt
from matplotlib import colormaps as cmaps

class ColorGradient:

    def __init__(self, colors):

        if len(colors) < 2:
            raise ValueError('Must supply at least two colors!')

        self.lenc = len(colors[0])
        for c in colors:
            if len(c) != self.lenc:
                raise ValueError('All color tuples must have equal lenght!')

        self.nc = len(colors)
        self.colors = [c for c in colors]
        self.bins = [i / (self.nc - 1) for i in range(self.nc)]
        self.binwidth = 1 / (self.nc - 1)

    def __len__(self):
        return self.nc

    def __call__(self, v):

        if v < 0:
            v = 0
        elif v > 1:
            v = 1

        if v == 0:
            return self.colors[0]
        elif v == 1:
            return self.colors[-1]

        else:
            for i in range(self.nc):
                if v < self.bins[i]:
                    c1 = self.colors[i - 1]
                    lim1 = self.bins[i - 1]
                    c2 = self.colors[i]
                    break

        return tuple(((c2[i] - c1[i]) / self.binwidth) * (v - lim1) + c1[i] for i in range(self.lenc))

class Colormap2D:

    def __init__(self, gradients):
        if len(gradients) < 2:
            raise ValueError('Must supply at least two gradients!')

        self.leng = len(gradients[0])
        for g in gradients:
            if len(g) != self.leng:
                raise ValueError('All color tuples must have equal lenght!')

        self.ng = len(gradients)
        self.gradients = [g for g in gradients]
        self.bins = [i / (self.ng - 1) for i in range(self.ng)]
        self.binwidth = 1 / (self.ng - 1)

    def __call__(self, v1, v2):

        if v2 < 0:
            v2 = 0
        if v2 > 1:
            v2 = 1

        if v2 == 0:
            g1 = self.gradients[0]
            g2 = self.gradients[0]
            norm_v2 = 0
        elif v2 == 1:
            g1 = self.gradients[-1]
            g2 = self.gradients[-1]
            norm_v2 = 1
        else:
            for i in range(self.ng):
                if v2 < self.bins[i]:
                    g1 = self.gradients[i - 1]
                    g2 = self.gradients[i]
                    norm_v2 = (v2 / self.binwidth) - self.bins[i - 1] / (self.bins[i] - self.bins[i - 1])
                    break


        return ColorGradient([g1(v1), g2(v1)])(norm_v2)

grad1 = ColorGradient([(0, 0, 1), (1, 1, 1), (1, 0, 0)])
grad2 = ColorGradient([(0, 1, 1), (0.75, 1, 0.75), (1, 1, 0)])
grad3 = ColorGradient([(0, 1, 0), (0, 1, 1), (1, 0, 1)])

cmap2d = Colormap2D([grad1, grad2, grad3])

def mod(x):
    return 1 # np.exp(-10 * x**2) - 1

def f1(x, z):
    return np.sin(x + z) * np.cos(x - z) * np.sin(x * z) / z

def f2(x, z):
    return z * np.sin(x * z) * np.cos(x**2 * z)**2

flist = [f1, f2]
def f(x, z):
    return (sum(fi(x, z) for fi in flist) + (z - 0.5)) * mod(x)

def g1(x, z):
    return sin(x * z) * cos(x - 3 * z)**2

def g2(x, z):
    return cos(x * (z - 0.5)) * cos(x * (z - 0.5))**3 + 0.5 * (z - 0.5)

glist = [g1, g2]
def g(x, z):
    return sum(gi(x, z) for gi in glist) * 5 * (z - 0.5)

def h1(x, z):
    return -cos(x**2 * (z - 0.5)**3) * cos(x * (z - 0.5))**3 - 0.5 * (z - 0.5)

def h2(x, z):
    return cos((x - sin(x)) * z) * (sin(x + cos(x)))**2

hlist = [h1, h2]
def h(x, z):
    return sum(hi(x, z) for hi in hlist) * 2

if __name__ == '__main__':
    xl = np.linspace(-3, 3, 200)

    plt.figure(figsize=(10, 5))

    xlims1 = lambda z : int(len(xl) * z / 3) + int(len(xl) / 6)
    xlims2 = lambda z : int(len(xl) * z * 2 / 3)
    nz = [100, 200, 200]
    funclist = [f, h, g]
    color_xshift = [0.8, 1, 0.6]
    alpha_list = [0.05, 0.05, 0.05]
    for fi, func in enumerate(funclist):
        zl = np.linspace(0, 1, nz[fi])
        for z in zl:
            for i in range(xlims1(z)):
                plt.plot(xl[i : i + 2], func(xl[i : i + 2], z), color=cmap2d((xl[i] / max(xl)) / color_xshift[fi], z)
                         , alpha=alpha_list[fi])
    nz = [200, 100, 200]
    funclist = [h, f, g]
    color_xshift = [1, 0.8, 0.6]
    alpha_list = [0.05, 0.05, 0.05]
    for fi, func in enumerate(funclist):
        zl = np.linspace(0, 1, nz[fi])
        for z in zl:
            for i in range(xlims1(z), xlims2(z)):
                plt.plot(xl[i : i + 2], func(xl[i : i + 2], z), color=cmap2d((xl[i] / max(xl)) / color_xshift[fi], z)
                         , alpha=alpha_list[fi])

    nz = [200, 200, 100]
    funclist = [h, g, f]
    color_xshift = [1, 0.6, 0.8]
    alpha_list = [0.05, 0.05, 0.05]
    for fi, func in enumerate(funclist):
        zl = np.linspace(0, 1, nz[fi])
        for z in zl:
            for i in range(xlims2(z), len(xl) - 1):
                plt.plot(xl[i : i + 2], func(xl[i : i + 2], z), color=cmap2d((xl[i] / max(xl)) / color_xshift[fi], z)
                         , alpha=alpha_list[fi])

    # NOTE: Because I couldn't figure out how to completely remove the green background from the header, the background
    #       figure needs to have white backing (not be transparent). Someone that knows more CSS than me can probably
    #       just remove the green background in `style.css` or something.
    ax = plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.get_xaxis().set_ticks([])
    ax.get_yaxis().set_ticks([])
    plt.gca().set_facecolor('white')

    plt.xlim(min(xl), max(xl))
    plt.ylim(-2, 3)
    plt.savefig('header.pdf', bbox_inches='tight', pad_inches=0)
    plt.show()