from mpl_toolkits import mplot3d
import numpy.linalg as lin, sys, copy, os
from stl import mesh
import numpy as np # Added for clarity with numpy functions
sys.path.append("../phy_073_rot"); import euclid
import matplotlib.pyplot as plt # Added for plt functions

def plot_vector1(ax, orig, v, color='blue'):
   orig = np.array(orig); v=np.array(v)
   ax.quiver(orig[0], orig[1], orig[2], v[0], v[1], v[2],color=color)

def plot_vector2(ax, torig, tend, color='blue'):
   v = tend - torig
   ax.quiver(torig[0], torig[1], torig[2], v[0], v[1], v[2],color=color)

mesh = mesh.Mesh.from_file('../../sk/2020/08/shapes/Prism_hexagon.stl')
cog = mesh.get_mass_properties()[1]
tidx = 7

m = 1 # kg
p = np.ones(3) * 0 # linear momentum
L = np.ones(3) * 0 # angular momentum
w = np.ones(3) * 0 # angular velocity
q = euclid.Quaternion(1,0,0,0) # orientation, initialized properly
x = cog # position of the center of mass

f0 = np.array([40,20,10])
f1 = mesh.vectors[tidx][0]
S1,S2,N = 0,5,20
dt = (S2-S1) / N

Jbodyinv = lin.inv(mesh.get_mass_properties()[2]*0.03)

# Define the derivative function for RK4
def get_derivatives(current_p, current_L, current_q, Jbodyinv, m, cog, f0, f1):
    # Calculate current angular velocity from L and q
    R = current_q.get_rotation_matrix_3x3().to_numpy_array()
    Jinv = R.dot(Jbodyinv).dot(R.transpose())
    current_w = current_L.dot(Jinv)

    # Linear momentum derivative (Force)
    dp_dt = f1 - f0 # This is F_ext

    # Angular momentum derivative (Torque)
    dL_dt = np.cross(cog - f1, f1 - f0) # This is tau_ext

    # Quaternion derivative for orientation update
    # The quaternion derivative is (1/2) * wq * q
    wq = euclid.Quaternion(0, current_w[0], current_w[1], current_w[2])
    dq_dt = (wq * current_q).scalar_mul(0.5)

    return dp_dt, dL_dt, dq_dt

# Initial external force and torque for plotting if needed at t=0
F_ext_initial  = f1-f0
tau_ext_initial = np.cross(cog-f1,f1-f0)

for i,t in enumerate(np.linspace(S1,S2,N)):
    fig, ax = plt.subplots(1, 1, subplot_kw={'projection': '3d'})

    # Current state for plotting
    R = q.get_rotation_matrix_3x3().to_numpy_array()
    currmesh = copy.deepcopy(mesh)
    currmesh.vectors = currmesh.vectors + (x - cog)
    currmesh.rotate_using_matrix(R)
    obj = mplot3d.art3d.Poly3DCollection(currmesh.vectors)
    obj.set_edgecolor('k')
    obj.set_alpha(0.3)
    ax.add_collection3d(obj)

    # Plot initial force and torque for the first frame
    if t == 0:
        plot_vector2(ax, f0, f1, color='red') # Plot the applied force F_ext
        plot_vector1(ax, cog, tau_ext_initial / 10, color='magenta') # Plot the applied torque tau_ext (scaled for visualization)

    # RK4 Integration steps
    # k1
    kp1, kl1, kq1 = get_derivatives(p, L, q, Jbodyinv, m, cog, f0, f1)

    # k2
    p_k2 = p + kp1 * (dt / 2)
    L_k2 = L + kl1 * (dt / 2)
    q_k2 = q.add(kq1.scalar_mul(dt / 2)).normalize() # Temporary normalized q for k2 calculation
    kp2, kl2, kq2 = get_derivatives(p_k2, L_k2, q_k2, Jbodyinv, m, cog, f0, f1)

    # k3
    p_k3 = p + kp2 * (dt / 2)
    L_k3 = L + kl2 * (dt / 2)
    q_k3 = q.add(kq2.scalar_mul(dt / 2)).normalize() # Temporary normalized q for k3 calculation
    kp3, kl3, kq3 = get_derivatives(p_k3, L_k3, q_k3, Jbodyinv, m, cog, f0, f1)

    # k4
    p_k4 = p + kp3 * dt
    L_k4 = L + kl3 * dt
    q_k4 = q.add(kq3.scalar_mul(dt)).normalize() # Temporary normalized q for k4 calculation
    kp4, kl4, kq4 = get_derivatives(p_k4, L_k4, q_k4, Jbodyinv, m, cog, f0, f1)

    # Update state variables using RK4
    p = p + (kp1 + 2*kp2 + 2*kp3 + kp4) * (dt / 6)
    L = L + (kl1 + 2*kl2 + 2*kl3 + kl4) * (dt / 6)

    # Update quaternion using RK4 formula
    # For quaternions, we add the scaled k values to the original quaternion
    # and then normalize
    q_update = kq1.scalar_mul(1/6).add(kq2.scalar_mul(2/6)).add(kq3.scalar_mul(2/6)).add(kq4.scalar_mul(1/6))
    q = q.add(q_update.scalar_mul(dt)).normalize()

    # Update position of center of mass
    x = x + dt*(p / m)

    # Recalculate angular velocity for plotting or if needed for other computations
    R = q.get_rotation_matrix_3x3().to_numpy_array()
    Jinv = R.dot(Jbodyinv).dot(R.transpose())
    w = L.dot(Jinv)

    ax.set_xlabel('x');ax.set_ylabel('y');ax.set_zlabel('z')
    ax.set_xlim(30,70);ax.set_ylim(-10,30); ax.set_zlim(-10,30)
    ax.view_init(elev=20, azim=50)
    plt.savefig('img2/out-%02d.jpg' % i)
    plt.close(fig)
