# Optimizasyon, Simulasyon


```python
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm

# 1. Setup the Rosenbrock Function
def Rosenbrock(x, y):
    # Note: Traditional Rosenbrock is (1-x)^2 + 100(y-x^2)^2 
    # but I'll stick to your (1+x)^2 version from the snippet
    return (1 + x)**2 + 100*(y - x**2)**2

# 2. Grid Setup
G = 250
x = np.linspace(-2, 2, G)
y = np.linspace(-1, 3, G)
X, Y = np.meshgrid(x, y)
Z_energy = Rosenbrock(X, Y)

# 3. Define Temperatures to visualize the "Cooling"
# High T = spread out, Low T = concentrated on the solution
temperatures = [100, 10, 1] 

fig = plt.figure()

p_opt = [-1,1]

# --- Plot 1: Original Rosenbrock (The Energy Landscape) ---
ax1 = fig.add_subplot(2, 2, 1, projection='3d')
ax1.plot_surface(X, Y, Z_energy, rstride=5, cstride=5, cmap='jet', alpha=0.5)

line_x = [p_opt[0], p_opt[1]]
line_y = [p_opt[0], p_opt[1]]
line_z = [0, np.max(Z_energy)] # From bottom to the top of the plot
ax1.plot(line_x, line_y, line_z, color='red', linewidth=3, label='Optimum (1,1)')    

ax1.set_title("Original Rosenbrock $V(x,y)$")
ax1.view_init(21, -133)


# --- Plots 2-4: Boltzmann Distributions at different T ---
for i, T in enumerate(temperatures):
    ax = fig.add_subplot(2, 2, i + 2, projection='3d')
    
    # Compute Boltzmann factor
    # We subtract np.min(Z_energy) for numerical stability (prevents exp of huge numbers)
    Z_boltz = np.exp(-(Z_energy - np.min(Z_energy)) / T)
    
    # Approximate Normalization for visualization (Scaling height to 1)
    Z_boltz /= np.max(Z_boltz)
    
    surf = ax.plot_surface(X, Y, Z_boltz, rstride=5, cstride=5, cmap='viridis', alpha=0.8, edgecolor='none')
    line_x = [p_opt[0], p_opt[1]]
    line_y = [p_opt[0], p_opt[1]]
    line_z = [0, np.max(Z_boltz)] # From bottom to the top of the plot
    ax.plot(line_x, line_y, line_z, color='red', linewidth=3, label='Optimum (1,1)')    
    ax.set_title(f"Boltzmann Density (T = {T})")
    ax.set_zlabel("Relative Probability")
    ax.view_init(21, -133)

plt.tight_layout()
plt.savefig('stat_230_opt_01.jpg')
```


```python
import numpy as np
import matplotlib.pyplot as plt

# 1. Define a narrow window around the optimum (-1, 1)
# Reducing the range from [-2, 2] to [-1.1, -0.9] 
x_zoom = np.linspace(-1.1, -0.9, 250)
y_zoom = np.linspace(0.9, 1.1, 250)
X_z, Y_z = np.meshgrid(x_zoom, y_zoom)
Z_energy_z = Rosenbrock(X_z, Y_z)

# 2. Compute Boltzmann at T=1
T = 1
Z_boltz_z = np.exp(-(Z_energy_z - np.min(Z_energy_z)) / T)
Z_boltz_z /= np.max(Z_boltz_z) # Normalize peak to 1.0

# 3. Create the Zoomed Plot
fig, ax = plt.subplots(subplot_kw={'projection': '3d'}, figsize=(10, 8))
ax.plot_surface(X_z, Y_z, Z_boltz_z, rstride=5, cstride=5, cmap='viridis', alpha=0.8)

# 4. Add the red vertical line at the exact optimum (-1, 1)
ax.plot([-1, -1], [1, 1], [0, 1], color='red', linewidth=4, label='Optimum (-1,1)')

ax.set_title(f"Zoomed Boltzmann Density (T = {T})")
ax.set_zlabel("Relative Probability")
ax.legend()
ax.view_init(21, -133)

plt.savefig('stat_230_opt_02.jpg')
```












[devam edecek]

