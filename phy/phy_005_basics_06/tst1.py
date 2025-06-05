import numpy as np

def rk4_step(func, dt, t, y):
    k1 = dt * func(t, y)
    k2 = dt * func(t + 0.5 * dt, y + 0.5 * k1)
    k3 = dt * func(t + 0.5 * dt, y + 0.5 * k2)
    k4 = dt * func(t + dt, y + k3)
    return y + (k1 + 2*k2 + 2*k3 + k4) / 6

def rb_motion(t, X):
    x, y_pos, vx, vy = X

    m = 1     # topun kutlesi

    dx_dt = vx
    dy_dt = vy
    dvx_dt = 0.0 
    dvy_dt = -g  

    return np.array([dx_dt, dy_dt, dvx_dt, dvy_dt])
