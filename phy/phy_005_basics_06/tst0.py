import numpy as np
import matplotlib.pyplot as plt

def rk4_step(func, dt, t, y):
    k1 = dt * func(t, y)
    k2 = dt * func(t + 0.5 * dt, y + 0.5 * k1)
    k3 = dt * func(t + 0.5 * dt, y + 0.5 * k2)
    k4 = dt * func(t + dt, y + k3)
    return y + (k1 + 2*k2 + 2*k3 + k4) / 6

def projectile_motion_eom(t, X):
    x, y_pos, vx, vy = X

    g = 9.81  # yercekimi
    m = 1     # topun kutlesi

    dx_dt = vx
    dy_dt = vy
    dvx_dt = 0.0 
    dvy_dt = -g  

    return np.array([dx_dt, dy_dt, dvx_dt, dvy_dt])

m = 1.0
initial_force = 500.0  # baslangic kuvveti, Newton 
force_direction = np.array([1.0, 1.0])
force_direction = force_direction / np.linalg.norm(force_direction) 

dt = 0.01  # zaman dilimi (saniye)

# Initial position [x0, y0]
x0 = 0.0
y0 = 0.0

initial_acceleration = initial_force / m * force_direction
initial_velocity_magnitude = initial_force / m 
                                                                                                                                             
initial_velocity_vector = initial_velocity_magnitude * force_direction
vx0 = initial_velocity_vector[0]
vy0 = initial_velocity_vector[1]

print(f"Baslangic Hizi: [{vx0:.2f} m/s, {vy0:.2f} m/s]")

initial_state = np.array([x0, y0, vx0, vy0])

time_points = [0.0]
state_history = [initial_state]
current_state = initial_state
current_time = 0.0

while current_state[1] >= 0:
    current_state = rk4_step(projectile_motion_eom, dt, current_time, current_state)
    current_time += dt
    if current_state[1] < 0: break

    time_points.append(current_time)
    state_history.append(current_state)

