import matplotlib.pyplot as plt
import numpy as np
import plot_phi
import time

# initial function phi - level set is a square 4 pixels
# away from borders on each side, in 3D it looks like an empty
# box
c0=4; w=4
nrow, ncol= (80,80)
phi=c0*np.ones((nrow,ncol))
phi[w+1:-w-1, w+1:-w-1]=-c0
plot_phi.plot_phi(phi)

dt=1.

iter=0

plt.ion()

while iter < 20:
    # gradient of phi
    gradPhiY, gradPhiX = np.gradient(phi)
    # magnitude of gradient of phi
    absGradPhi=np.sqrt(gradPhiX**2+gradPhiY**2)                               
    dPhiBydT = 1 * absGradPhi     
    # level set evolution equation    
    phi = phi + ( dt * dPhiBydT )
    iter=iter+1
    time.sleep(0.6)
    plt.hold(False)
    CS = plt.contour(phi,0, colors='r') 
    plt.draw()
    plt.hold(False)
    iter += 1
