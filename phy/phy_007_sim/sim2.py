from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import random
from PIL import Image
from PIL import ImageOps
import numpy as np
import sys

G = np.array([0.0, 0.0, -9.8])
m = 0.1

class Simulation:
    def __init__(self):
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
        v = np.array([0.0, 0.0, 0.0])
        p = np.array([0.8, 0.0, 0.9])
        f = np.array([-40.0, 0.0, 0.0])
        self.balls.append({'pos':p, 'f':f, 'v': v})
        
        v = np.array([0.0, 0.0, 0.0])
        p = np.array([-0.8, -0.0, 0.9])
        f = np.array([40.0, 0.0, 0.0])
        self.balls.append({'pos':p, 'f':f, 'v': v})
                
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
                
        if self.i > 300: exit()
        
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
            if j==0:
                glMaterialfv(GL_FRONT, GL_DIFFUSE, [1.0, 0.0, 0.0, 1.0])
            if j==1:
                glMaterialfv(GL_FRONT, GL_DIFFUSE, [0.0, 0.0, 1.0, 1.0])
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

