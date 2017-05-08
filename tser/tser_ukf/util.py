from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from matplotlib.patches import Ellipse
import math
import numpy as np
from numpy import eye, zeros, dot, isscalar, outer
from scipy.linalg import inv, cholesky
from numpy.random import randn
from scipy.stats import norm, multivariate_normal
import scipy.linalg as linalg
from matplotlib.patches import Ellipse,Arrow



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

