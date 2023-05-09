#include <cstdlib>
#include <stdlib.h>
#include "glut.h"
#include "Particle.h"
#include "V_Randomizer.h"
#include "shapes.h"

V_Randomizer randy;

/////// Number of particles ///////
int numberOfParticles = 500;
Particle** part;
float ambientLight[4] = { .1, .1, .1, 1.0 };
float diffuseLight[4] = { 0.5, 0.4,0.6, 1.0 };
float specularLight[4] = { 1, 1, 1, 1.0 };
float lightPos[4] = { 5, 5, 5, 1 };
float pos[3] = {0,-21,0};

float angleY = 0;
float angleSpeed = 0.5;
float updateSpeed = 0.08;

void light(){
	float lightPos2[4] = {5,20,0,1};
	float light2Dir[3] = {0,0,0};
	glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
	glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLight);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLight);
	glLightfv(GL_LIGHT0, GL_SPECULAR, specularLight);	
}

void display(void){
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
	light();
	glLoadIdentity();
	gluLookAt(20,20,-45,0,1,0,0,1,0);
	glRotatef(angleY,0,1,0);
	glPushMatrix();
	drawRoom(pos,25,2,25);
	glPopMatrix();

	for (int i = 0; i < numberOfParticles; i++){
		glPushMatrix();
		glRotatef(angleY,0,1,0);
		part[i]->Render();
		glPopMatrix();
	}
	glFlush();
	glutSwapBuffers();
}

void keys(unsigned char key, int x, int y){
	switch(key){

		// Change update speed
		case 's':
			updateSpeed -= 0.005;
			glutPostRedisplay();
			break;
		case 'd':
			updateSpeed += 0.005;
			glutPostRedisplay();
			break;

			// Change speed of camera rotation
		case 'i':
			angleSpeed+= 0.1;
			glutPostRedisplay();
			break;
		case 'o':
			angleSpeed -= 0.1;
			glutPostRedisplay();
			break;
	}
}
void menuParticleType(int value){
	switch(value){
		case 1:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setParticleType(1);
					glPopMatrix();
				}
			break;
		case 2:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setParticleType(2);
					glPopMatrix();
				}
			break;
			case 3:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(3);
							glPopMatrix();
						}
			break;

			case 4:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(4);
							glPopMatrix();
						}
			break;

			case 5:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(5);
							glPopMatrix();
						}
			break;

			case 6:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(6);
							glPopMatrix();
						}
			break;

			case 7:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(7);
							glPopMatrix();
						}
			break;
			case 8:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(8);
							glPopMatrix();
						}
			break;

			case 9:
				for (int i = 0; i < numberOfParticles; i++){
							part[i]->setParticleType(rand()%7+1);
							glPopMatrix();
						}
			break;
	}
}

void menuMain(int value){
	if (value == 5)
		exit(0);
}
void menuColour(int value){
	switch(value){
		case 1:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(1,0,0);
				}
			break;
		case 2:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(0,1,0);
				}
			break;
		case 3:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(0,0,1);
				}
			break;
		case 4:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(1,1,0);
				}
			break;
		case 5:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(0,1,1);
				}
			break;
		case 6:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(1,0,1);
				}
			break;
		case 7:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(0.1,0.1,0.1);
				}
			break;

		case 8:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setColour(randy.box_muller(0,1),randy.box_muller(0,1),randy.box_muller(0,1));
				}
			break;
	}
}

void menuLight(int value){
	switch(value){
		case 1:
			glEnable(GL_LIGHT0);
			break;
		case 2:
			glDisable(GL_LIGHT0);
			break;
	}
}

void menuSize(int value){
	switch(value){
		case 1:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(1);
				}
			break;
		case 2:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(2);
				}
			break;
		case 3:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(3);
				}
			break;
		case 4:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(4);
				}
			break;
		case 5:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(5);
				}
			break;
		case 6:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(6);
				}
			break;
		case 7:
			for (int i = 0; i < numberOfParticles; i++){
					part[i]->setSize(7);
				}
			break;
	}
}
void initMenus(){
	glutCreateMenu(menuParticleType);
	glutAddMenuEntry("Sphere", 1);
	glutAddMenuEntry("Teapot (slow)", 2);
	glutAddMenuEntry("Cube", 3);
	glutAddMenuEntry("Wire Sphere", 4);
	glutAddMenuEntry("Wire Cube", 5);
	glutAddMenuEntry("Cone", 6);
	glutAddMenuEntry("Donut", 7);
	glutAddMenuEntry("Dot", 8);
	glutAddMenuEntry("Random", 9);


	glutCreateMenu(menuColour);
	glutAddMenuEntry("Red", 1);
	glutAddMenuEntry("Green", 2);
	glutAddMenuEntry("Blue", 3);

	glutAddMenuEntry("Yellow", 4);
	glutAddMenuEntry("Teal", 5);
	glutAddMenuEntry("Purple", 6);
	glutAddMenuEntry("Black", 7);

	glutAddMenuEntry("Random", 8);

	glutCreateMenu(menuSize);
	glutAddMenuEntry("1", 1);
	glutAddMenuEntry("2", 2);
	glutAddMenuEntry("3", 3);
	glutAddMenuEntry("4", 4);
	glutAddMenuEntry("5", 5);
	glutAddMenuEntry("6", 6);
	glutAddMenuEntry("7", 7);

	glutCreateMenu(menuLight);
	glutAddMenuEntry("On", 1);
	glutAddMenuEntry("Off", 2);


	glutCreateMenu(menuMain);
	glutAddSubMenu("Shape", 1);
	glutAddSubMenu("Colour", 2);
	glutAddSubMenu("Size", 3);
	glutAddSubMenu("Light", 4);
	glutAddMenuEntry("Quit", 5);


	glutAttachMenu(GLUT_RIGHT_BUTTON);
}
void update(int value){

	angleY+= angleSpeed;
	for (int i = 0; i < numberOfParticles; i++){
		part[i]->Update(updateSpeed);
	}
	glutPostRedisplay();
	glutTimerFunc(10,update,0);
}
void init(void){
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glClearColor(1,1,1,1);

	glFrontFace(GL_CW);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_FRONT);
	gluPerspective(100,1,1,1000);
	glMatrixMode(GL_MODELVIEW);
	glEnable(GL_DEPTH_TEST);
	glShadeModel(GL_SMOOTH);
	glEnable (GL_BLEND); 
	glEnable(GL_COLOR_MATERIAL);
	glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

	initMenus();
	glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
}
int main(int argc, char** argv)
{

	part = new Particle*[numberOfParticles];

	for (int i = 0; i < numberOfParticles; i++){
		part[i] = new Particle	(0,-10.0,randy.box_muller(-0.5,0.5),//Position
								randy.box_muller(-1,1), randy.box_muller(30,34),randy.box_muller(-1,1),//Velocity
								randy.box_muller(0,1),randy.box_muller(0,1),randy.box_muller(0,1),  //Colour
								1); // Alpha
								
}

	glutInit(&argc, argv);		//starts up GLUT
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutInitWindowSize(600,600);
	glutCreateWindow("Particle Fountain");	//creates the window
	glutDisplayFunc(display);	
	glutKeyboardFunc(keys);
	init();

	glutTimerFunc(25,update,0);

	glutMainLoop();				//starts the event loop
	
	return(0);					//return may not be necessary on all compilers
}