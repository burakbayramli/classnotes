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
        self.rt = False
        print ('self.mmax',self.mmax)
        print ('self.mmin',self.mmin)
        
    def init(self):
        v = np.array([0.0, 0.0, 0.0])
        p = np.array([0.8, 0.0, 0.9])
        f = np.array([-40.0, 0.0, 0.0])
        self.balls.append({'pos':p, 'f':f, 'v': v})
        
        v = np.array([0.0, 0.0, 0.0])
        p = np.array([-0.8, 0.0, 0.9])
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
                print ('i==1 f',j,b['f'])
                #exit()
        else: 
            for b in self.balls:
                b['f'] = G * m
                        
    def integrate(self):
        print ('ball 0 v',self.balls[0]['v'])
        print ('ball 1 v',self.balls[1]['v'])
        for j,b in enumerate(self.balls):
            print ('bv',j,b['v'])
            print ('v addition',j,self.dt*(b['f']/m))
            b['v'] += self.dt*(b['f']/m)
            print ('av',j,b['v'])
            b['pos'] += self.dt*b['v']

            #print (j, b['pos'])            
            
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
                    
        print ('\n')


            
    def update(self):
        self.computeForces()
        self.integrate()
        glutPostRedisplay()

    def display(self):
        glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glPushMatrix()
        glRotatef(self.th,0.0,1.0,0.0)
        glRotatef(90.0,-1.0,0.0,0.0)
        glutWireCube(2.0)
        for b in self.balls:
            glPushMatrix()
            glTranslatef(b['pos'][0],b['pos'][1],b['pos'][2])
            glutSolidSphere(self.r,50,50)
            glPopMatrix()
        glPopMatrix()
        glutSwapBuffers()

	# her 40'inci resmi diske png olarak yaz
        if self.i % 40 == 0: 
            width,height = 640,480
            data = glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE)
            image = Image.frombytes("RGBA", (width, height), data)
            image = ImageOps.flip(image)
            image.save('/tmp/glutout-%03d.png' % self.i, 'PNG')
        self.i += 1
        
    def mouse(self,button,state,x,y):
        if button == GLUT_LEFT_BUTTON:
            rt = not state
        elif button == GLUT_RIGHT_BUTTON:
            sys.exit(0)

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


