# https://c4science.ch/diffusion/3852/browse/master/test.py
# convert -loop 0 -delay 100 frames1/*.png frames1/cart.gif
from scipy.integrate import odeint
import control, gym, time
import numpy as np
from numpy import sin, cos

import matplotlib.pyplot as plt
import numpy as np
import math
import time

l_bar = 2.0  # length of bar
l = l_bar 
m = 0.3  # [kg]
g = 9.8  # [m/s^2]
g = 9.8
m = 0.3
M = 1.0
l = 1.0
mu = 0.1 # friction
Q = np.array([[100., 0., 0., 0.],[0, 1, 0, 0],[0, 0, 1000, 0],[0, 0, 0, 1]] )
R = 0.0001
#init_theta = -5.0
init_theta = 0.5
x0 = [0.0, 0.0, init_theta, 2.0]

def flatten(a):
    return np.array(a).flatten()

def show_cart(fout, xt, theta):
    cart_w = 1.0
    cart_h = 0.5
    radius = 0.1

    cx = np.matrix([-cart_w / 2.0, cart_w / 2.0, cart_w /
                    2.0, -cart_w / 2.0, -cart_w / 2.0])
    cy = np.matrix([0.0, 0.0, cart_h, cart_h, 0.0])
    cy += radius * 2.0

    cx = cx + xt

    bx = np.matrix([0.0, l_bar * math.sin(-theta)])
    bx += xt
    by = np.matrix([cart_h, l_bar * math.cos(-theta) + cart_h])
    by += radius * 2.0

    angles = np.arange(0.0, math.pi * 2.0, math.radians(3.0))
    ox = [radius * math.cos(a) for a in angles]
    oy = [radius * math.sin(a) for a in angles]

    rwx = np.copy(ox) + cart_w / 4.0 + xt
    rwy = np.copy(oy) + radius
    lwx = np.copy(ox) - cart_w / 4.0 + xt
    lwy = np.copy(oy) + radius

    wx = np.copy(ox) + float(bx[0, -1])
    wy = np.copy(oy) + float(by[0, -1])

    plt.figure()
    plt.plot(flatten(cx), flatten(cy), "-b")
    plt.plot(flatten(bx), flatten(by), "-k")
    plt.plot(flatten(rwx), flatten(rwy), "-k")
    plt.plot(flatten(lwx), flatten(lwy), "-k")
    plt.plot(flatten(wx), flatten(wy), "-k")

    #plt.axis("equal")
    plt.xlim(-3, 3)
    plt.savefig(fout)
        
A = np.array([[0, 0, 1, 0],
    [0, 0, 0, 1],
    [0, -(M/m)*g, -mu/m, 0],
    [0, (m+M)*g/m*l, mu/m*l, 0]])
    
B = np.array([[0], [0], [1/m],[-1/m*l]])

K,X,e = control.lqr(A,B,Q,R);

print (K)
Fs = []
def f(y, t):
    x1,x2,x3,x4 = y
    xs = np.array([1,0,0,0])
    F = np.float(np.dot(K,(xs - np.array([x1,x2,x3,x4]))))
    Fs.append(F)
    tmp1 = (F - M*g*sin(2*x2)/2 + M*l*x4**2*sin(x2) - mu*x3)/(M*sin(x2)**2 + m)
    tmp2 = (g*(M + m)*sin(x2) - (F + M*l*x4**2*sin(x2) - mu*x3)*cos(x2))/(l*(M*sin(x2)**2 + m))
    return [x3, x4, tmp1, tmp2 ]


t = np.linspace(0, 5, 100)
sol = odeint(f, x0, t)
#print (sol[:,0])
#print (sol[:,1])

for i,row in enumerate(sol):
    if i % 5 == 0:
        print (row[0], row[1])
        show_cart('frames1/cart-%04d' % i, row[0], row[1])
        #show_cart(-3.4, -0.5)
