from scipy.integrate import odeint
import control, gym, time
import numpy as np
from numpy import sin, cos

env = gym.make('CartPole-v0').env

x = env.reset()

#curr_theta = 0
#for i in range(10):
#    observation, reward, done, info = env.step(1)
#    curr_theta = observation[2]

curr_theta = 0.0
    
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


N = 10. / tau
t = np.linspace(0, 10, N)
x0 = [0.0, 0.0, curr_theta, 2.0]
sol = odeint(f, x0, t)
print (sol[:,0])
print (sol[:,1])

print (env.force_mag)

for F in Fs:
    action = 0
    if F > 24000:
        action = 0
    else:
        action = 1
    print (F,action)
    observation, reward, done, info = env.step(action)
    env.render()
    time.sleep(0.4)
