from random import random
from collections import defaultdict 
import numpy as np, datetime
import sys, numpy.linalg as lin
from mayavi import mlab

G = np.array([0.0, 0.0, -0.8])

m = 0.1
B = 10 # top

class Simulation:
    def __init__(self):
        self.i = 0
        #self.r   = 0.1
        self.g   = 9.8
        self.dt  = 0.3
        #self.cor = 0.6        
        self.cor = 1.0
        self.balls = []
        self.tm  = 0.0
        self.th  = 0.0
        self.R = 0.02
        self.mmax =  1.0-self.R
        self.mmin = 0+self.R
        self.right = False
        self.left = False
        
    def init(self):
        for b in range(B):
            v = np.array([0.0, 0.0, 0.0])
            p = np.array([np.random.rand(), np.random.rand(), np.random.rand()])
            f = 5*np.array([np.random.rand(), np.random.rand(), np.random.rand()])
            self.balls.append({'pos':p, 'f':f, 'v': v, 'i': b})
                        

    def computeForces(self):
        if (self.i==1):
            for j,b in enumerate(self.balls):
                b['f'] = b['f'] + (G * m)
        else: 
            for b in self.balls:
                b['f'] = G * m
                        
    def integrate(self):
        
        for j,b in enumerate(self.balls):
            b['v'] += self.dt*(b['f']/m)
            b['pos'] += self.dt*b['v']
            
            if (abs(b['pos'][0]) >= self.mmax):
                #print (b['i'], 'wall 1')
                b['v'][0] *= -self.cor
                if b['pos'][0] < 0:
                    b['pos'][0] = self.mmin

            if (abs(b['pos'][1]) >= self.mmax):
                #print (b['i'], 'wall 2')
                b['v'][1] *= -self.cor
                if b['pos'][1] < 0:
                    b['pos'][1] = self.mmin
                    
            if (abs(b['pos'][2]) >= self.mmax):
                #print (b['i'], 'wall 3')
                b['v'][2] *= -self.cor
                if b['pos'][2] < 0:
                    b['pos'][2] = self.mmin

        vDone = {}
        for j,b in enumerate(self.balls):
            for other in self.balls:
                if (other['i'] != b['i'] and b['i'] not in vDone and other['i'] not in vDone):
                    dist = lin.norm(other['pos']-b['pos'])
                    if (dist < (2*self.R)):
                        #print ('collision')
                        vrel = b['v']-other['v']
                        n = (other['pos']-b['pos']) / dist
                        vnorm = np.dot(vrel,n)*n
                        #print (vnorm)
                        b['v'] = b['v'] - vnorm
                        other['v'] = other['v'] + vnorm                            
                        vDone[b['i']] = 1
                        vDone[other['i']] = 1
                            
            
            
    def update(self):
        self.computeForces()
        self.integrate()
            
    def display(self, i):
        outp = np.array([[0.,0.,0.],[1.,1.,1.]])
        mlab.options.offscreen = True
        self.r = np.ones(len(self.balls)) * 0.03
        mlab.figure(fgcolor=(0., 0., 0.), bgcolor=(1, 1, 1))
        ball_vect = [[b['pos'][0],b['pos'][1],b['pos'][2]] for b in self.balls]
        ball_vect = np.array(ball_vect)
        #print (ball_vect)
        #print ('\n')
        mlab.points3d(ball_vect[:, 0], ball_vect[:, 1], ball_vect[:, 2], self.r, scale_mode='none', color=(0.2, 0.4, 0.5))
        mlab.points3d(outp[:, 0], outp[:, 1], outp[:, 2], [0.0001,0.00001], scale_mode='none', color=(1,1,1))
        mlab.outline()
        mlab.savefig(filename='/tmp/sim/out-%02d.png' % i) 

if __name__ == '__main__':
    s = Simulation()
    s.init()
    for i in range(40):
        s.update()
        s.display(i)
        #exit()
