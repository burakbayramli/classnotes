from random import random
from collections import defaultdict 
import numpy as np, datetime
import sys, numpy.linalg as lin
from mayavi import mlab
import itertools

m = 0.1
R = 0.1
EPS = 0.1
BOUND_DAMPING = -0.6
mmin,mmax=0.0,2.0
BN = int(np.abs(mmax-mmin) / R) + 1
bins = np.linspace(mmin, mmax, BN)
G = np.array([0.0, 0.0, -9.8*2])

REST_DENS = 10.0
GAS_CONST = 0.5
MASS = 100.0
VISC = 20.0
DT = 0.1
H = 0.1 # kernel radius
PI = 3.14159
HSQ = H*H # radius^2 for optimization
POLY6 = 315.0/(65.0*PI*np.power(H, 9.));
SPIKY_GRAD = -45.0/(PI*np.power(H, 6.));
VISC_LAP = 45.0/(PI*np.power(H, 6.));
EPS = 0.05
BOUND_DAMPING = -0.5


idx27 = list(itertools.product( [-1,0,1], repeat=3  ))

class Simulation:
    def __init__(self):
        self.r   = R
        self.balls = []
        
    def init(self):
        i = 0
        for xs in np.linspace(0, 0.4, 10):
            for ys in np.linspace(0, 0.4, 10):
                for zs in np.linspace(0, 0.4, 10):
                    v = np.array([0.0, 0.0, 0.0])
                    f = np.array([0,0,0])
                    x = np.array([xs, ys, zs])
                    xi = np.digitize(xs, bins)
                    yi = np.digitize(ys, bins)
                    zi = np.digitize(zs, bins)                    
                    d = {'x': x, 'f':f, 'v': v, 'i': i, 'rho': 0.0, 'p': 0.0, 'grid': (xi,yi,zi)}
                    self.balls.append(d)
                    i += 1
        self.rvec   = np.ones(i) * self.r

    def get_neighbors(self, ball):
        neighbors = {}
        cx,cy,cz = ball['grid']
        for (xa,ya,za) in idx27:
            nx,ny,nz = cx+xa,cy+ya,cz+za
            if (nx,ny,nz) in self.grid_hash:
                tn = self.grid_hash[(nx,ny,nz)]
                for n in tn: neighbors[ n['i'] ] = n
        return neighbors
        
    def hash_balls(self):
        self.grid_hash = defaultdict(list)
        for i,b in enumerate(self.balls):
            xi = np.digitize(b['x'][0], bins)
            yi = np.digitize(b['x'][1], bins)
            zi = np.digitize(b['x'][2], bins)
            b['grid'] = (xi,yi,zi)
            self.grid_hash[(xi,yi,zi)].append(b)
            
    def computeDensityPressure(self):
        for i,pi in enumerate(self.balls):            
            pi['rho'] = 0.0                
            otherList = self.get_neighbors(pi)
            for (k,pj) in otherList.items():
                r2 = lin.norm(pj['x']-pi['x'])**2
                if  r2 < HSQ:
                    pi['rho'] += MASS*POLY6*np.power(HSQ-r2, 3.0)
            pi['p'] = GAS_CONST*(pi['rho'] - REST_DENS)
       
                
    def computeForces(self):
        for i,pi in enumerate(self.balls):
            fpress = np.array([0.0, 0.0, 0.0])
            fvisc = np.array([0.0, 0.0, 0.0])                
            otherList = self.get_neighbors(pi)
            for k,pj in otherList.items():
                if pj['i'] == pi['i']: continue
                rij = pi['x']-pj['x']
                r = lin.norm(rij)
                if r < H:
                    if np.sum(rij)>0.0: rij = rij / r
                    tmp1 = -rij*MASS*(pi['p'] + pj['p']) / (2.0 * pj['rho'])
                    tmp2 = SPIKY_GRAD*np.power(H-r,2.0)
                    fpress += (tmp1 * tmp2)
                    tmp1 = VISC*MASS*(pj['v'] - pi['v'])
                    tmp2 = pj['rho'] * VISC_LAP*(H-r)
                    fvisc += (tmp1 / tmp2)
            fgrav = G * pi['rho']
            pi['f'] = fpress + fvisc + fgrav
                        
    def integrate(self):
        for j,p in enumerate(self.balls):
            if p['rho'] > 0.0: 
                p['v'] += DT*p['f']/p['rho']
            p['x'] += DT*p['v']

            if p['x'][0]-EPS < 0.0:
                p['v'][0] *= BOUND_DAMPING
                p['x'][0] = 0.0
            if p['x'][0]+EPS > 2.0:
                p['v'][0] *= BOUND_DAMPING
                p['x'][0] = 2.0-EPS

            if p['x'][1]-EPS < 0.0:
                p['v'][1] *= BOUND_DAMPING
                p['x'][1] = 0.0
            if p['x'][1]+EPS > 2.0:
                p['v'][1] *= BOUND_DAMPING
                p['x'][1] = 2.0-EPS

            if p['x'][2]-EPS < 0.0:
                p['v'][2] *= BOUND_DAMPING
                p['x'][2] = 0.0
            if p['x'][2]+EPS > 2.0:
                p['v'][2] *= BOUND_DAMPING
                p['x'][2] = 2.0-EPS


        self.hash_balls()
                

    def update(self):
        self.hash_balls()
        self.computeDensityPressure()
        self.computeForces()
        self.integrate()                    
        
    def display(self, i):
        mlab.options.offscreen = True
        ball_vect = [[b['x'][0],b['x'][1],b['x'][2]] for b in self.balls]
        ball_vect = np.array(ball_vect)

        fig = mlab.figure(figure=None, fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1), engine=None)
        color=(0.2, 0.4, 0.5)
        mlab.points3d(ball_vect[:,0], ball_vect[:,1], ball_vect[:,2], self.rvec, color=color, colormap = 'gnuplot', scale_factor=1, figure=fig)
        mlab.points3d(0, 0, 0, 0.1, color=(1,0,0), scale_factor=1.0)
        
        BS = 2.0
        mlab.plot3d([0.0,0.0],[0.0, 0.0],[0.0, BS], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([0.0,BS],[0.0, 0.0],[0.0, 0.0], color=(1,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([0.0,0.0],[0.0, BS],[0.0, 0.0], color=(0,1,0), tube_radius=None, figure=fig)
        mlab.plot3d([0.0,0.0],[0.0, BS],[BS, BS], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([0.0,BS],[0.0,0.0],[BS,BS], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([BS,BS],[0.0,BS],[BS,BS], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([BS,0],[BS,BS],[BS,BS], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([0,0],[BS,BS],[BS,0], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([BS,BS],[0.0,0.0],[0.0,BS], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([BS,BS],[0.0,BS],[0.0,0.0], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([BS,0.0],[BS,BS],[0.0,0.0], color=(0,0,0), tube_radius=None, figure=fig)
        mlab.plot3d([BS,BS],[BS,BS],[0.0,BS], color=(0,0,0), tube_radius=None, figure=fig)

        mlab.view(azimuth=50, elevation=80, focalpoint=[1, 1, 1], distance=8.0, figure=fig)
        
        mlab.savefig(filename='/tmp/sim/out-%02d.png' % i)

if __name__ == '__main__':
    s = Simulation()
    s.init()
    for i in range(20):
        print (i)
        s.update()
        s.display(i)
        #exit()
