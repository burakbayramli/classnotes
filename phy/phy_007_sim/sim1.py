from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import random
from PIL import Image
from PIL import ImageOps
import sys

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
        p = [0.5,0.1,0.9]
        v = [-1,-1,-1]
        self.balls.append({'pos':p,'vel':v})
        
        p = [0.1,0.9,0.9]
        v = [1,0.5,-1]
        self.balls.append({'pos':p,'vel':v})        
                
        tm = 0.0

        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glEnable(GL_DEPTH_TEST)
        glClearColor(1.0,1.0,1.0,1.0)

        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluPerspective(60.0,1.0,1.0,50.0)
        glTranslatef(0.0, 0.0, -3.5)
        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()

    def update(self):
        for b in self.balls:
            b['vel'][2] += -self.g*self.dt
            b['pos'][0] += b['vel'][0]*self.dt
            b['pos'][1] += b['vel'][1]*self.dt
            b['pos'][2] += b['vel'][2]*self.dt

            if (abs(b['pos'][0]) >= self.mmax):
                b['vel'][0] *= -self.cor
                if b['pos'][0] < 0:
                    b['pos'][0] = self.mmin
                else:
                    b['pos'][0] = self.mmax

            if (abs(b['pos'][1]) >= self.mmax):
                b['vel'][1] *= -self.cor
                if b['pos'][1] < 0:
                    b['pos'][1] = self.mmin
                else:
                    b['pos'][1] = self.mmax

            if (abs(b['pos'][2]) >= self.mmax):
                b['vel'][2] *= -self.cor
                if b['pos'][2] < 0:
                    b['pos'][2] = self.mmin
                else:
                    b['pos'][2] = self.mmax

        if self.right:
            self.th += 0.2
            if self.th>360.0:
                self.th -= 360.0

        if self.left:
            self.th -= 0.2
            if self.th>360.0:
                self.th -= 360.0

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


