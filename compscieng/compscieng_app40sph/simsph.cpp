// g++ simsph.cpp -lX11 -lGL -lGLU -lglut -g -Wall -O2 -o islet.exe
// https://github.com/cerrno/mueller-sph
#include <GL/glut.h>
#include <iostream>
#include <fstream> 
#include <vector>
using namespace std;

#include <eigen3/Eigen/Dense>
using namespace Eigen;

// "Particle-Based Fluid Simulation for Interactive Applications"
// solver parameters
const static Vector3d G(0.f, 0.f, 12000*-9.8f); // external (gravitational) forces
const static float REST_DENS = 1000.f; // rest density
const static float GAS_CONST = 2000.f; // const for equation of state
const static float H = 16.f; // kernel radius
const static float HSQ = H*H; // radius^2 for optimization
const static float MASS = 65.f; // assume all particles have the same mass
const static float VISC = 250.f; // viscosity constant
const static float DT = 0.0008f; // integration timestep

// smoothing kernels defined in Müller and their gradients
const static float POLY6 = 315.f/(65.f*M_PI*pow(H, 9.f));
const static float SPIKY_GRAD = -45.f/(M_PI*pow(H, 6.f));
const static float VISC_LAP = 45.f/(M_PI*pow(H, 6.f));

// simulation parameters
const static float EPS = 0.05; // boundary epsilon
const static float BOUND_DAMPING = -0.5f;

// particle data structure
// stores position, velocity, and force for integration
// stores density (rho) and pressure values for SPH
struct Particle {
    Particle(float _x, float _y, float _z) : x(_x,_y,_z),
				   v(0.f,0.f, 0.f),
				   f(0.f,0.f, 0.f),
				   rho(0),
				   p(0.f) {}
    Vector3d x, v, f;
    float rho, p;
};

// solver data
static vector<Particle> particles;

// interaction
const static int MAX_PARTICLES = 2500;
const static int DAM_PARTICLES = 500;
const static int BLOCK_PARTICLES = 250;

// rendering projection parameters
const static int WINDOW_WIDTH = 800;
const static int WINDOW_HEIGHT = 600;
//const static double VIEW_WIDTH = 1.5*800.f;
//const static double VIEW_HEIGHT = 1.5*600.f;

void InitSPH(void)
{
    for(float y = 0; y < 0.2; y += 0.05)	
	for(float x = 0; x <= 0.2; x += 0.05)	    
	    for(float z = 0; z <= 0.2; z += 0.05)	    
		particles.push_back(Particle(x,y,z));
}

void Integrate(void)
{
    cout << "integrating" << endl;
    for(auto &p : particles)
    {
	// forward Euler integration
	p.v += DT*p.f/p.rho;
	p.x += DT*p.v;

	// enforce boundary conditions
	if(p.x(0)-EPS < -1.0f)
        {
	    p.v(0) *= BOUND_DAMPING;
	    p.x(0) = EPS;
        }
	if(p.x(0)+EPS > 1.0f) 
        {
	    p.v(0) *= BOUND_DAMPING;
	    p.x(0) = 1.0-EPS;
        }
	
	if(p.x(1)-EPS < -1.0f)
        {
	    p.v(1) *= BOUND_DAMPING;
	    p.x(1) = EPS;
        }
	if(p.x(1)+EPS > 1.0f)
        {
	    p.v(1) *= BOUND_DAMPING;
	    p.x(1) = 1.0-EPS;
        }

	if(p.x(2)-EPS < -1.0f)
        {
	    p.v(2) *= BOUND_DAMPING;
	    p.x(2) = EPS;
        }
	if(p.x(2)+EPS > 1.0f)
        {
	    p.v(2) *= BOUND_DAMPING;
	    p.x(2) = 1.0-EPS;
        }
	
    }
}

void ComputeDensityPressure(void)
{
    for(auto &pi : particles)
    {
	pi.rho = 0.f;
	for(auto &pj : particles)
        {
	    Vector3d rij = pj.x - pi.x;
	    float r2 = rij.squaredNorm();

	    if(r2 < HSQ)
            {
		// this computation is symmetric
		pi.rho += MASS*POLY6*pow(HSQ-r2, 3.f);
            }
        }
	pi.p = GAS_CONST*(pi.rho - REST_DENS);
    }
}

void ComputeForces(void)
{
    for(auto &pi : particles)
    {
	Vector3d fpress(0.f, 0.f, 0.f);
	Vector3d fvisc(0.f, 0.f, 0.f);
	for(auto &pj : particles)
        {
	    if(&pi == &pj)
		continue;

	    Vector3d rij = pj.x - pi.x;
	    float r = rij.norm();

	    if(r < H)
            {
		// compute pressure force contribution
		fpress += -rij.normalized()*MASS*(pi.p + pj.p)/(2.f * pj.rho) * SPIKY_GRAD*pow(H-r,2.f);
		// compute viscosity force contribution
		fvisc += VISC*MASS*(pj.v - pi.v)/pj.rho * VISC_LAP*(H-r);
            }
        }
	Vector3d fgrav = G * pi.rho;
	pi.f = fpress + fvisc + fgrav;
    }
}

void Update(void)
{ 
    ComputeDensityPressure();
    ComputeForces();
    Integrate();

    glutPostRedisplay();
}

void InitGL(void)
{
//    glClearColor(0.9f,0.9f,0.9f,1);
//    glEnable(GL_POINT_SMOOTH);
//    glPointSize(H/2.f);
//    glMatrixMode(GL_PROJECTION);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_DEPTH_TEST);
    glClearColor(1.0,1.0,1.0,1.0);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(60.0,1.0,1.0,50.0);
    glTranslatef(0.0,0.0,-3.5);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
}

void Render(void)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glPushMatrix();
    glRotatef(0,0.0,1.0,0.0);
    glRotatef(90.0,-1.0,0.0,0.0);
    glutWireCube(2.0);

    glBegin(GL_POINTS);
    for(auto &p : particles){
	glPushMatrix();
	glTranslatef(p.x(0), p.x(1), p.x(2));
	GLfloat mat_ambient[] ={0.0, 0.0, 1.0, 1.0};
	glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_ambient);
	glutSolidSphere(0.05,50,50);
	glPopMatrix();
    }
    glPopMatrix();
    
    glEnd();

    glutSwapBuffers();

}

int main(int argc, char** argv)
{
    InitSPH();
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    //glutInitWindowSize(500,500);
    glutInitWindowSize(WINDOW_WIDTH,WINDOW_HEIGHT);
    glutCreateWindow("SPH");
    glutDisplayFunc(Render);
    glutIdleFunc(Update);
    InitGL();

    glutMainLoop();
    return 0;
}
