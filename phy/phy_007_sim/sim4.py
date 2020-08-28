from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import random
from PIL import Image
from PIL import ImageOps
from collections import defaultdict 
import numpy as np, datetime
import sys, numpy.linalg as lin

p1,p2,p3 = 73856093, 19349663, 83492791
G = np.array([0.0, 0.0, -9.8])

m = 0.1
B = 3 # balls
l = 0.2 # hash box size
n = B*10 # size of hash table

def spatial_hash(x):
    ix,iy,iz = np.floor((x[0]+2.0)/l), np.floor((x[1]+2.0)/l), np.floor((x[2]+2.0)/l)
    return (int(ix*p1) ^ int(iy*p2) ^ int(iz*p3)) % n

class Simulation:
    def __init__(self):
        self.geo_hash_list = None
        self.i = 0
        self.n   = 2
        self.r   = 0.1
        self.g   = 9.8
        self.dt  = 0.01
        self.cor = 0.6
        self.balls = []
        self.tm  = 0.0
        self.th  = 0.0
        self.mmax =  1.0-self.r
        self.mmin = -1.0+self.r
        self.right = False
        self.left = False
        
    def init(self):
        for b in range(B):
            v = np.array([0.0, 0.0, 0.0])
            p = np.array([np.random.rand(), np.random.rand(), 0.9])
            f = 30*np.array([np.random.rand(), np.random.rand(), 0.0])
            self.balls.append({'pos':p, 'f':f, 'v': v, 'i': b})
                
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

    def computeForces(self):
        if (self.i==1):
            for j,b in enumerate(self.balls):
                b['f'] = b['f'] + (G * m)
        else: 
            for b in self.balls:
                b['f'] = G * m
                        
    def integrate(self):
        self.geo_hash_list = defaultdict(list)
        
        for j,b in enumerate(self.balls):
            b['v'] += self.dt*(b['f']/m)
            b['pos'] += self.dt*b['v']
            
            if (abs(b['pos'][0]) >= self.mmax):
                b['v'][0] *= -self.cor
                if b['pos'][0] < 0:
                    b['pos'][0] = self.mmin

            if (abs(b['pos'][1]) >= self.mmax):
                b['v'][1] *= -self.cor
                if b['pos'][1] < 0:
                    b['pos'][1] = self.mmin
                    
            if (abs(b['pos'][2]) >= self.mmax):
                b['v'][2] *= -self.cor
                if b['pos'][2] < 0:
                    b['pos'][2] = self.mmin

        for j,b in enumerate(self.balls):
            self.geo_hash_list[spatial_hash(self.balls[j]['pos'])].append(self.balls[j])

        for j,b in enumerate(self.balls):
            if (len(self.geo_hash_list[spatial_hash(self.balls[j]['pos'])])>1):
                otherList = self.geo_hash_list[spatial_hash(self.balls[j]['pos'])]
                for other in otherList:
                    if (other['i'] != b['i']):
                        dist = lin.norm(other['pos']-b['pos'])
                        if (dist<2*self.r): print (dist)
            
            
    def update(self):
        self.computeForces()
        self.integrate()

        if self.right:
            self.th += 0.2
            if self.th>360.0:
                self.th -= 360.0

        if self.left:
            self.th -= 0.2
            if self.th>360.0:
                self.th -= 360.0
                
        if self.i > 800: exit()
        
        glutPostRedisplay()

    def display(self):
        glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glPushMatrix()
        glRotatef(self.th,0.0,1.0,0.0)
        glRotatef(90.0,-1.0,0.0,0.0)
        glutWireCube(2.0)
        for j,b in enumerate(self.balls):
            glPushMatrix()
            glTranslatef(b['pos'][0],b['pos'][1],b['pos'][2])
            #glMaterialfv(GL_FRONT, GL_DIFFUSE, [np.random.rand(), np.random.rand(), np.random.rand(), 1.0])
            glutSolidSphere(self.r,50,50)
            glPopMatrix()
        glPopMatrix()
        glutSwapBuffers()

	# her 40'inci resmi diske png olarak yaz
        if self.i % 20 == 0: 
            width,height = 480,480
            data = glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE)
            image = Image.frombytes("RGBA", (width, height), data)
            image = ImageOps.flip(image)
            image.save('/tmp/glutout-%03d.png' % self.i, 'PNG')
        self.i += 1
        
    def mouse(self,button,state,x,y):
        if button == GLUT_LEFT_BUTTON:
            self.right = not state
        elif button == GLUT_RIGHT_BUTTON:
            self.left = not state

if __name__ == '__main__':
    s = Simulation()
    glutInit(())    
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH)
    glutInitWindowSize(500,500)
    glutCreateWindow("GLUT Bouncing Ball in Python")
    glutDisplayFunc(s.display)
    glutIdleFunc(s.update)
    glutMouseFunc(s.mouse)
    s.init()
    glutMainLoop()

