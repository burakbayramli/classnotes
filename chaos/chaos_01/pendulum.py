import scipy as sp
import matplotlib.pyplot as plt
def pendulum(T,n,theta0,v0,alpha):
    dt = T/float(n)
    t = sp.linspace(0,T,n+1)
    v = sp.zeros(n+1)
    theta = sp.zeros(n+1)
    v[0] = v0
    theta[0] = theta0
    for k in range(n):
        theta[k+1] = theta[k] + dt*v[k]
        v[k+1] = v[k] - alpha*dt*sp.sin(theta[k+1])
        return theta, v, t


def test_values():
    theta0 = sp.pi/6
    n = 1000
    T = 10
    v0 = 0
    alpha = 5
    return T, n, theta0, v0, alpha
