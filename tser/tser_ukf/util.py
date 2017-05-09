from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
import math
import scipy.stats
import numpy as np
from numpy import eye, zeros, dot, isscalar, outer
from scipy.linalg import inv, cholesky
from numpy.random import randn
from scipy.stats import norm, multivariate_normal
import scipy.linalg as linalg
from matplotlib.patches import Ellipse, Arrow

def covariance_ellipse(P, deviations=1):
    U,s,v = linalg.svd(P)
    orientation = math.atan2(U[1,0],U[0,0])
    width  = deviations*math.sqrt(s[0])
    height = deviations*math.sqrt(s[1])
    assert width >= height
    return (orientation, width, height)

def plot_covariance_ellipse(mean, cov=None, variance = 1.0, std=None,
             ellipse=None, title=None, axis_equal=True, show_semiaxis=False,
             facecolor=None, edgecolor=None,
             fc='none', ec='#004080',
             alpha=1.0, xlim=None, ylim=None,ls='solid',plt=None):
    assert cov is None or ellipse is None
    assert not (cov is None and ellipse is None)

    if facecolor is None:facecolor = fc
    if edgecolor is None: edgecolor = ec
    if cov is not None:ellipse = covariance_ellipse(cov)
    if axis_equal:plt.axis('equal')
    if title is not None:plt.title (title)
    compute_std = False
    if std is None:
        std = variance
        compute_std = True

    if np.isscalar(std):std = [std]

    if compute_std:std = np.sqrt(np.asarray(std))

    ax = plt.gca()

    angle = np.degrees(ellipse[0])
    width = ellipse[1] * 2.
    height = ellipse[2] * 2.

    for sd in std:
        e = Ellipse(xy=mean, width=sd*width, height=sd*height, angle=angle,
                    facecolor=facecolor,
                    edgecolor=edgecolor,
                    alpha=alpha,
                    lw=2, ls=ls)
        ax.add_patch(e)
    x, y = mean
    plt.scatter(x, y, marker='+', color=edgecolor) # mark the center
    if xlim is not None:
        ax.set_xlim(xlim)

    if ylim is not None:
        ax.set_ylim(ylim)

    if show_semiaxis:
        a = ellipse[0]
        h, w = height/4, width/4
        plt.plot([x, x+ h*cos(a+np.pi/2)], [y, y + h*sin(a+np.pi/2)])
        plt.plot([x, x+ w*cos(a)], [y, y + w*sin(a)])

def arrow(x1,y1,x2,y2, width=0.2):
    return Arrow(x1,y1, x2-x1, y2-y1, lw=1, width=width, ec='k', color='k')

def plot_bivariate_colormap(xs, ys):
    xs = np.asarray(xs)
    ys = np.asarray(ys)
    xmin = xs.min()
    xmax = xs.max()
    ymin = ys.min()
    ymax = ys.max()
    values = np.vstack([xs, ys])
    kernel = scipy.stats.gaussian_kde(values)
    X, Y = np.mgrid[xmin:xmax:100j, ymin:ymax:100j]
    positions = np.vstack([X.ravel(), Y.ravel()])

    Z = np.reshape(kernel.evaluate(positions).T, X.shape)
    plt.gca().imshow(np.rot90(Z), cmap=plt.cm.Greys,
                     extent=[xmin, xmax, ymin, ymax])


def plot_monte_carlo_mean(xs, ys, f, mean_fx, label, plot_colormap=True):
    fxs, fys = f(xs, ys)

    computed_mean_x = np.average(fxs)
    computed_mean_y = np.average(fys)
    plt.subplot(121)
    plt.gca().grid(b=False)

    plot_bivariate_colormap(xs, ys)

    plt.scatter(xs, ys, marker='.', alpha=0.02, color='k')
    plt.xlim(-20, 20)
    plt.ylim(-20, 20)

    plt.subplot(122)
    plt.gca().grid(b=False)

    plt.scatter(fxs, fys, marker='.', alpha=0.02, color='k')
    plt.scatter(mean_fx[0], mean_fx[1],
                marker='v', s=300, c='r', label=label)
    plt.scatter(computed_mean_x, computed_mean_y,
                marker='*',s=120, c='b', label='Hesaplanan Ortalama')

    plot_bivariate_colormap(fxs, fys)
    plt.ylim([-10, 200])
    plt.xlim([-100, 100])
    plt.legend(loc='best', scatterpoints=1)
    print ('Ortalamalardaki fark x={:.3f}, y={:.3f}'.format(
           computed_mean_x-mean_fx[0], computed_mean_y-mean_fx[1]))
