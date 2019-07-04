import numpy as np
import scipy as sp
from numpy import cos, sin
from scipy.integrate.odepack import odeint

m1=1.0
m2=1.0
L1=1.0
L2=1.0
g=9.8

def f(z, t):
    z1, z2, z3, z4 = z
    tmp1 = (-m2*L1*z4**2*sin(z1-z2)*cos(z1-z2)+g*m2*sin(z2)*cos(z1-z2)-m2*L2*z4**2*sin(z1-z2)-(m1+m2)*g*sin(z1))/(L1*(m1+m2)-m2*L1*cos(z1-z2)**2);
    tmp2 = (m2 * L2 * (z4**2) *sin(z1-z2) * cos(z1-z2) + g*sin(z1)*cos(z1-z2)*(m1+m2) + L1*(z4**2) * sin(z1-z2)*(m1+m2)-g*sin(z2)*(m1+m2))/(L2*(m1+m2)-m2*L2*cos(z1-z2)**2)
    z = [ z3, z4, tmp1, tmp2 ]
    return z

z0 = np.array([1.0, 1.0, 1.0, 1.0])
t = np.linspace(0, 10, 100)
z = odeint(f, z0, t)
x1=L1*sin(z[:,0])
y1=-L1*cos(z[:,0])
x2=L1*sin(z[:,0])+L2*sin(z[:,1])
y2=-L1*cos(z[:,0])-L2*cos(z[:,1])

