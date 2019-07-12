from scipy.integrate import odeint
import control, gym
import numpy as np
from numpy import sin, cos

env = gym.make('CartPole-v0').env

x = env.reset()

#for i in range(5): env.step(0)
for i in range(5): env.step(1)

g = env.gravity
m = env.masspole
M = env.masscart
l = env.length
tau = env.tau

mu = 0.1 # friction
Q = np.array([[100., 0., 0., 0.],[0, 1, 0, 0],[0, 0, 1000, 0],[0, 0, 0, 1]] )
R = 0.0001

A = np.array([[0, 0, 1, 0],
    [0, 0, 0, 1],
    [0, -(m/M)*g, -mu/M, 0],
    [0, (m+M)*g/M*l, mu/M*l, 0]])
    
B = np.array([[0], [0], [1/M],[-1/M*l]])

K,X,e = control.lqr(A,B,Q,R);

print (K)


def f(y, t):
    x1,x2,x3,x4 = y
    xs = np.array([1,0,0,0])
    F = np.float(np.dot(K,(xs - np.array([x1,x2,x3,x4]))))
    tmp1 = (F - M*g*sin(2*x2)/2 + M*l*x4**2*sin(x2) - mu*x3)/(M*sin(x2)**2 + m)
    tmp2 = (g*(M + m)*sin(x2) - (F + M*l*x4**2*sin(x2) - mu*x3)*cos(x2))/(l*(M*sin(x2)**2 + m))
    return [x3, x4, tmp1, tmp2 ]


N = 10. / tau
t = np.linspace(0, 10, N)
x0 = [0.0, 0.0, -np.pi, 2.0]
sol = odeint(f, x0, t)
print (sol)
