import control
import gym
import numpy as np

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
