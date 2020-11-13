from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import random
from PIL import Image
from PIL import ImageOps
from collections import defaultdict 
import numpy as np, datetime, itertools
import sys, numpy.linalg as lin

p1,p2,p3 = 73856093, 19349663, 83492791
G = np.array([0.0, 0.0, -9.8*2])

B = 10 # top
R = 0.1
mmin,mmax=-1.0,+1.0
BN = int(np.abs(mmax-mmin) / R) + 1
bins = np.linspace(mmin, mmax, BN)

idx27 = list(itertools.product( [-1,0,1], repeat=3  ))

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
img = True


class Simulation:
    def __init__(self):
        self.grid_hash = None
        self.i = 0
        self.r   = 0.05
        self.balls = []
        self.tm  = 0.0
        self.th  = 200.0
        
    def init(self):
        i = 0
        for xs in np.linspace(-0.3, 0.3, 10):
            for ys in np.linspace(-0.3, 0.3, 10):
                for zs in np.linspace(0.0, 0.2, 4):
                    v = np.array([0.0, 0.0, 0.0])
                    f = np.array([0,0,0])
                    x = np.array([xs, ys, zs])
                    xi = np.digitize(xs, bins)
                    yi = np.digitize(ys, bins)
                    zi = np.digitize(zs, bins)                    
                    d = {'x': x, 'f':f, 'v': v, 'i': i, 'rho': 0.0, 'p': 0.0, 'grid': (xi,yi,zi)}
                    self.balls.append(d)
                    i += 1

                        
        tm = 0.0

        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glEnable(GL_DEPTH_TEST)
        glClearColor(1.0,1.0,1.0,1.0)

        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluPerspective(60.0,1.0,1.0,50.0)
        glTranslatef(0.0,0.0,-3.5)
        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()

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

            if p['x'][0]-EPS < -1.0:
                p['v'][0] *= BOUND_DAMPING
                p['x'][0] = -1.0
            if p['x'][0]+EPS > 1.0:
                p['v'][0] *= BOUND_DAMPING
                p['x'][0] = 1.0-EPS

            if p['x'][1]-EPS < -1.0:
                p['v'][1] *= BOUND_DAMPING
                p['x'][1] = -1.0
            if p['x'][1]+EPS > 1.0:
                p['v'][1] *= BOUND_DAMPING
                p['x'][1] = 1.0-EPS

            if p['x'][2]-EPS < -1.0:
                p['v'][2] *= BOUND_DAMPING
                p['x'][2] = -1.0
            if p['x'][2]+EPS > 1.0:
                p['v'][2] *= BOUND_DAMPING
                p['x'][2] = 1.0-EPS


        self.hash_balls()
                
            
    def update(self):
        self.hash_balls()
        self.computeDensityPressure()
        self.computeForces()
        self.integrate()                    
        if self.i > 40: exit()
        glutPostRedisplay()

    def display(self):
        glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glPushMatrix()
        glRotatef(self.th,0.0,1.0,0.0)
        glRotatef(90.0,-1.0,0.0,0.0)
        glutWireCube(2.0)
        for j,b in enumerate(self.balls):
            glPushMatrix()
            glTranslatef(b['x'][0],b['x'][1],b['x'][2])
            glMaterialfv(GL_FRONT, GL_DIFFUSE, [0.0, 0.0, 1.0, 1.0])
            glutSolidSphere(self.r,50,50)
            glPopMatrix()
        glPopMatrix()
        glutSwapBuffers()

        if img and self.i % 2 == 0: 
            width,height = 480,480
            data = glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE)
            image = Image.frombytes("RGBA", (width, height), data)
            image = ImageOps.flip(image)
            image.save('/tmp/glut/glutout-%03d.png' % self.i, 'PNG')
                               
        self.i += 1

if __name__ == '__main__':
    if (os.path.exists("/tmp/glut") == False): os.mkdir("/tmp/glut")
    s = Simulation()
    glutInit(())    
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH)
    glutInitWindowSize(500,500)
    glutCreateWindow("GLUT Bouncing Ball in Python")
    glutDisplayFunc(s.display)
    glutIdleFunc(s.update)
    s.init()
    glutMainLoop()

