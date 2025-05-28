# convert -scale 30% /tmp/sim/*.png /tmp/balls6.gif
from random import random
from collections import defaultdict 
import numpy as np, datetime
import sys, numpy.linalg as lin
from mayavi import mlab

G = np.array([0.0, 0.0, -0.8])

m = 0.1
B = 8 # top

EPS = 0.1
BOUND_DAMPING = -0.6

class Simulation:
    def __init__(self):
        self.r   = 0.2
        self.rvec   = np.ones(B) * self.r
        self.dt  = 0.1
        self.balls = []
        self.cor = 0.5
        self.mmax =  2.0-self.r
        self.mmin = 0.0+self.r
        
    def init(self):
        for b in range(B):
            v = np.array([0.0, 0.0, 0.0])
            p = np.array([np.random.rand(), np.random.rand(), np.random.rand()])
            f = 5*np.array([np.random.rand(), np.random.rand(), np.random.rand()])
            self.balls.append({'x':p, 'f':f, 'v': v, 'i': b})
                        

    def computeForces(self, i):
        if (i==0):
            for j,b in enumerate(self.balls):
                b['f'] = b['f'] + (G * m)
        else: 
            for b in self.balls:
                b['f'] = G * m
                        
    def integrate(self):
        
        for j,p in enumerate(self.balls):
            p['v'] += self.dt*(p['f']/m)
            p['x'] += self.dt*p['v']
            
            if p['x'][0]-EPS < 0:
                p['v'][0] *= BOUND_DAMPING
                p['x'][0] = 0
            if p['x'][0]+EPS > 2.0:
                p['v'][0] *= BOUND_DAMPING
                p['x'][0] = 2.0-EPS

            if p['x'][1]-EPS < 0:
                p['v'][1] *= BOUND_DAMPING
                p['x'][1] = 0
            if p['x'][1]+EPS > 2.0:
                p['v'][1] *= BOUND_DAMPING
                p['x'][1] = 2.0-EPS

            if p['x'][2]-EPS < 0:
                p['v'][2] *= BOUND_DAMPING
                p['x'][2] = 0
            if p['x'][2]+EPS > 2.0:
                p['v'][2] *= BOUND_DAMPING
                p['x'][2] = 2.0-EPS


        vDone = {}
        for j,b in enumerate(self.balls):
            for other in self.balls:
                if (other['i'] != b['i'] and b['i'] not in vDone and other['i'] not in vDone):
                    dist = lin.norm(other['x']-b['x'])
                    if (dist < (2*self.r)):
                        #print ('collision')
                        vrel = b['v']-other['v']
                        n = (other['x']-b['x']) / dist
                        vnorm = np.dot(vrel,n)*n
                        #print (vnorm)
                        b['v'] = b['v'] - vnorm
                        other['v'] = other['v'] + vnorm                            
                        vDone[b['i']] = 1
                        vDone[other['i']] = 1
                            
            
            
    def update(self,i):
        self.computeForces(i)
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
        #exit()

if __name__ == '__main__':
    s = Simulation()
    s.init()
    for i in range(40):
        s.update(i)
        s.display(i)
        #exit()
