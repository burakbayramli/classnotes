#include "Particle.h"
#include "glut.h"
#include "V_Randomizer.h"

Particle::~Particle(){ }

Particle::Particle(double _posX, double _posY, double _posZ, double _velX, double _velY, double _velZ, float colR, float colG, float colB, float colA){

    partType = 1;
    size = 1;

    pos.x = _posX;
    pos.y = _posY;
    pos.z = _posZ;

    vel.x = _velX;
    vel.y = _velY;
    vel.z = _velZ;

    acc.x = 0.0;
    acc.y = -9.8;
    acc.z = 0.0;

    colour[0]=colR;
    colour[1]=colG;
    colour[2]=colB;

    alpha = colA;
    age = 0;
    partType =1;

}
void Particle::setColour(float r,float g, float b){
    colour[0] = r;
    colour[1] = g;
    colour[2] = b;
}
void Particle::setSize(float newSize){
    size = newSize;
}
void Particle::Update(double time){

    V_Randomizer randNumber;
    pos.x += (vel.x * time) + (0.5 * (acc.y * (time * time)));
    vel.x = vel.x + (acc.x * time);
    pos.y += (vel.y * time) + (0.5 * (acc.y * (time * time)));
    vel.y = vel.y + (acc.y * time);
    pos.z += (vel.z * time) + (0.5 * (acc.y * (time * time)));
    vel.z = vel.z + (acc.z * time);

    //Boundaries
    if (pos.y <=-25){
	vel.y = 35;
	pos.x=0;
	pos.z=0;
    }

}

void Particle::setParticleType(int a){
    partType = a;
}
void Particle::Render(){

    V_Randomizer randNumber;
    glPushMatrix();

    glColor4f(colour[0],colour[1],colour[2],alpha);
    glTranslated(pos.x,pos.y,pos.z);
    glRotated(1.0,1.0,1.0,0.0);
    switch (partType){
    case 1:
	glutSolidSphere(size,15,15);
	break;
    case 2:
	glutSolidTeapot(size);
	break;
    case 3:
	glutSolidCube(size);
	break;
    case 4:
	glutWireSphere(size,15,15);
	break;
    case 5:
	glutWireCube(size);
	break;
    case 6:
	glutSolidCone(size,size,15,15);
	break;
    case 7:
	glutSolidTorus(size/2,size,15,15);
	break;
    case 8:
	glPointSize(size*2);
	glBegin(GL_POINTS);
	glVertex3f(0,0,0);
	glEnd();
	break;
    }
    glPopMatrix();
}

