#! /usr/local/bin/python3
# https://gist.github.com/jeiting/c381e195d6153eaf657c21f691c2e456
# Iterative LQR
import gym
import numpy as np

env = gym.make('CartPole-v0').env

x = env.reset()
# x, xdot, theta, thetadot

#for i in range(5): env.step(0)
for i in range(5): env.step(1)

gamma = (4.0 / 3.0 - env.masspole / env.total_mass)

a = -env.gravity * env.masspole / (env.total_mass * gamma)
b = (1.0 / env.total_mass * (1 + env.masspole / (env.total_mass * gamma)))
c = env.gravity / (env.length * gamma)
d = -1.0 / (env.total_mass * env.length * gamma)

tau = env.tau

F = np.array([
    [1, tau,       0,   0,       0],
    [0,   1, tau * a,   0, tau * b],
    [0,   0,       1, tau,       0],
    [0,   0, tau * c,   1, tau * d],
  ])

C = np.array([
    [1,  0, 0,  0,   0],
    [0,  0, 0,  0,   0],
    [0,  0, 1,  0,   0],
    [0,  0, 0,  0,   0],
    [0,  0, 0,  0,   1],
  ])

c = np.array([0, 0, 0, 0, 0]).T

frame = 0
done = False
i = 0
while 1:
    i += 1
    Ks = []
    T = 100
    # V = np.zeros((4, 4))
    # v = np.zeros((4))
    V = C[:4, :4]
    v = np.zeros((4))
    for t in range(T, -1, -1):
        # Qt
        Qt = C + np.matmul(F.T, np.matmul(V, F))
        qt = c + np.matmul(F.T, v)


        Quu = Qt[-1:,-1:]
        Qux = Qt[-1:,:-1]
        Qxu = Qt[:-1, -1:]

        qu = qt[-1:]

        Qut_inv = np.linalg.inv(Quu)

        Kt = -np.matmul(Qut_inv, Qux)
        kt = -np.matmul(Qut_inv, qu)

        Ks.append((Kt, kt))

        V = Qt[:4, :4] + np.matmul(Qxu, Kt) + np.matmul(Kt.T, Qux) + np.matmul(Kt.T, np.matmul(Quu, Kt))
        v = qt[:4] + np.matmul(Qxu, kt) + np.matmul(Kt.T, qu) + np.matmul(Kt.T, np.matmul(Quu, kt))

        Kt, kt = Ks[-1]
        ut = np.matmul(Kt, x.reshape((1, -1)).T) + kt

    if ut > 0.0:
      ut = env.force_mag
      action = 1
    else:
      ut = -env.force_mag
      action = 0


    xu = np.hstack([x, ut])
    my_guess = np.matmul(F, xu.T)
    x, reward, done, info = env.step(action)

    frame += 1
    env.render()
print(frame)
