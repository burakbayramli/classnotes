"""
This program solves the restricted three body problem using SciPy's odeint.
The solution (x(t), y(t)) represents the position of a satellite moving
in the earth-moon plane under the influence of their gravity.  The earth
and moon are assumed to rotate around their common center of mass,
unaffected by the satellite.

Computation is made with SciPy's odeint (implementation of LSODE),
an adaptive order, adaptive step size code, based on the
Bashford/Adams-Moulton predictor-corrector schemes.

A rotating coordinate system is taken with units such that
the origin is at the center of mass and the moon is always at (1,0).
Temporal units are chosen so that one month = 2 pi.

The differential equations for the position (x, y) of the satellite are:

  x'' = x + 2 y' - b (x + 1)/c - a (x - b)/d
  y'' = y - 2 x' - b y/c - a y/d

where a is the mass ratio moon/(moon + earth), b = 1 - a,
c = ((x + a)^2 + y^2)^(3/2),  d = ((x - b)^2 + y^2)^(3/2).

We compute an Arenstorf orbit (periodic trajectory) taken from
Scientific Computing, by W. Gander, M. Gander, F. Kwok, section 10.3.5.
x(0) = 0.994, x'(x) = 0, y(0) = 0, y'(0) = -2.00158510637908.
The period is 17.06521656015796.
"""
import numpy as np
from matplotlib.pyplot import *
from matplotlib import animation
from scipy.integrate import odeint

# We write the ODE as a first order system for z0 = x, z1 = y, z2 = x', z3 = y'
massratio = 80.45 # earth-moon mass ratio
a = 1./(1. + massratio)
b = 1. - a
def f(z, t):
    (x, y, xp, yp) = z
    c = ((x + a)**2 + y**2)**1.5
    d = ((x - b)**2 + y**2)**1.5
    xpp = x + 2*yp - b*(x + a)/c - a*(x - b)/d
    ypp  = y - 2*xp - b*y/c - a*y/d
    return (xp, yp, xpp, ypp)

# Take t be an array of time coordinates at which to compute the solution,
# with t[0] taken as the initial time.
t = np.linspace(0., 25., 5000)
# initial data
y0 = (0.994, 0., 0., -2.00158510637908) 
# solve the IVP; y[i, :] is the solution vector at time t[i]
y, info = odeint(f, y0, t, full_output=True, printmessg=True)

print('number of timesteps:            {}'.format(info['nst'][-1]))
print('number of function evaluations: {}'.format(info['nfe'][-1]))

      
# plot the coordinates as a function of time
fig, ax = subplots(1, 1, figsize=(15, 15))
ax.plot(t, y[:, 0], t, y[:, 1], linewidth=3)
title('solution', fontsize=40)
show()

# plot the trajectory
fig, ax = subplots(1, 1, figsize=(15, 15))
ax.set_xlim(-1.5, 1.5)
ax.set_ylim(-1.5, 1.5)
ax.plot(y[:, 0], y[:, 1], color='black', linewidth=3)
title('trajectory', fontsize=40)
show()

# plot the timestep selection
plot(info['hu'])
title('timesteps', fontsize=40)
show()


# convert the trajectories to a fixed coordinate system
thm = t
the = t + np.pi
ths = t + np.arctan2(y[:,1], y[:,0])
rm = 1.
re = -1./massratio # distance of earth from center of mass
rs = np.sqrt(y[:, 0]**2 + y[:, 1]**2)
(xm, ym) = (rm*np.cos(thm), rm*np.sin(thm))
(xe, ye) = (re*np.cos(the), re*np.sin(the))
(xs, ys) = (rs*np.cos(ths), rs*np.sin(ths))

# animate
fig, ax = subplots(1, 1, figsize=(15, 15))
ax.set_xlim(-1.5, 1.5)
ax.set_ylim(-1.5, 1.5)
(moon,) = ax.plot([], [], marker='o', markersize=10, color='lightblue')
(earth,) = ax.plot([], [], marker='o', markersize=40, color='darkgreen')
(satellite,) = ax.plot([], [], marker='o', markersize=2, color='black')
timer = ax.text(-1.3, 1.3, '', fontsize=32)

def init_anim():
    moon.set_data([], [])
    earth.set_data([], [])
    satellite.set_data([], [])
    timer.set_text('t =')
    return (moon, earth, satellite, timer)
def frame(i):
    moon.set_data(xm[i], ym[i])
    earth.set_data(xe[i], ye[i])
    satellite.set_data(xs[0:i], ys[0:i])
    timer.set_text('t = {:5.2f}'.format(t[i]))
    return (moon, earth, satellite, timer)
anim = animation.FuncAnimation(fig, frame, init_func=init_anim,
        frames=len(t), interval=10, repeat=False, blit=True)
show()
