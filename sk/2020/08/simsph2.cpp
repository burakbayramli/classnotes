// How to compile, and other info
// https://burakbayramli.github.io/dersblog/sk/2020/08/sph.html
// g++ simsph1.cpp -lX11 -lGL -lGLU -lglut -g -O2 -o /tmp/a.exe; /tmp/a.exe
// https://github.com/cerrno/mueller-sph
#include <GL/glut.h>
#include <iostream>
#include <fstream> 
#include <vector>
using namespace std;

#include <eigen3/Eigen/Dense>
using namespace Eigen;

// kordinat sistemi 0,500 arasinda, grafikleme icin -1,+1 ile
// surekli tercume yapiyoruz.

const static Vector3d G(0.f, 0.f, 12000*-9.8f); 
const static float REST_DENS = 500.f; // rest density
const static float GAS_CONST = 500.f; // const for equation of state
const static float H = 16.f; 
const static float DIST = 10.f;
const static float HSQ = H*H; 
const static float MASS = 65.f;
const static float VISC = 250.f;
const static float DT = 0.001f; 

// puruzsuzlestirici cekirdek ve turevleri
const static float POLY6 = 315.f/(65.f*M_PI*pow(H, 9.f));
const static float SPIKY_GRAD = -45.f/(M_PI*pow(H, 6.f));
const static float VISC_LAP = 45.f/(M_PI*pow(H, 6.f));

const static float EPS = H;
const static float BOUND_DAMPING = -0.5f;

struct Particle {
    Particle(float _x, float _y, float _z) : x(_x, _y, _z),
				   v(0.f, 0.f, 0.f),
				   f(0.f, 0.f, 0.f),
				   rho(0.f),
				   p(0.f) {}
    Vector3d x, v, f;
    float rho, p;
};

static vector<Particle> particles;
static vector<Particle> glParticles;

static float MAX_COORD = 500;
static int WINDOW_WIDTH = 500;

float glc(float x) {
    float res = x*(2.f/MAX_COORD) -1.f;
    return res;
}

void InitSPH(void)
{
    int balls = 0;
    for(float x = 0.f; x < 60.f; x += 5.f) { 
	for(float y = 440.f; y < 500.f; y += 5.f) {	
	    for(float z = 0.f; z < 60.7f; z += 5.f) {
		particles.push_back(Particle(x,y,z));
		balls ++;
		glParticles.push_back(Particle(glc(x),glc(y),glc(z)));
	    }
	}
    }

    std::cout << "balls:" << balls << std::endl;
}

void Integrate(void)
{
    for(auto &p : particles)
    {
	// ileri Euler entegrasyonu
	if (p.rho > 0.0f) p.v += DT*p.f/p.rho;
	p.x += DT*p.v;

	// sinir sartlarini kontrol et
	if(p.x(0)-EPS < 0.0f)
        {
	    p.v(0) *= BOUND_DAMPING;
	    p.x(0) = 0.0f;
        }
	if(p.x(0)+EPS > 500.0f) 
        {
	    p.v(0) *= BOUND_DAMPING;
	    p.x(0) = 500.0f-EPS;
        }
	
	if(p.x(1)-EPS < 0.0f)
        {
	    p.v(1) *= BOUND_DAMPING;
	    p.x(1) = 0.0f;
        }
	if(p.x(1)+EPS > 500.0f)
        {
	    p.v(1) *= BOUND_DAMPING;
	    p.x(1) = 500.0f-EPS;
        }

	if(p.x(2)-EPS < 0.0f)
        {
	    p.v(2) *= BOUND_DAMPING;
	    p.x(2) = 0.0f;
        }
	if(p.x(2)+EPS > 500.0f)
        {
	    p.v(2) *= BOUND_DAMPING;
	    p.x(2) = 500.0f-EPS;
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

	    if(r < DIST)
            {
		fpress += -rij.normalized()*MASS*(pi.p + pj.p)/(2.f * pj.rho) * SPIKY_GRAD*pow(H-r,2.f);
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

int renderCount = 0;

void Render(void)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glPushMatrix();
    glRotatef(200.0f,0.0,1.0,0.0);
    glRotatef(90.0,-1.0,0.0,0.0);
    glutWireCube(2.0);
    for(int i=0; i < particles.size(); i++){
	glParticles[i].x[0] = glc(particles[i].x[0]);
	glParticles[i].x[1] = glc(particles[i].x[1]);
	glParticles[i].x[2] = glc(particles[i].x[2]);
    }
    for(auto &p : glParticles){
	glPushMatrix();
	glTranslatef(p.x(0), p.x(1), p.x(2));
	GLfloat mat_ambient[] ={0.f, 0.f, 1.f, 1.f};
	glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_ambient);
	glutSolidSphere(0.05f,50.f,50.f);
	glPopMatrix();
    }
    glPopMatrix();    
    glutSwapBuffers();

    // her 40'inci goruntuyu diske yazmak
    if (renderCount % 2 == 0) {
	int* buffer = new int[ WINDOW_WIDTH * WINDOW_WIDTH * 3 ];
	glReadPixels( 0, 0, WINDOW_WIDTH, WINDOW_WIDTH, GL_BGR, GL_UNSIGNED_BYTE, buffer );
	std::string fname = "/tmp/glut/gl2-out-" + std::to_string(renderCount) + ".tga";
	FILE   *out = fopen(fname.c_str(), "w");
	short  TGAhead[] = {0, 2, 0, 0, 0, 0, WINDOW_WIDTH, WINDOW_WIDTH, 24};
	fwrite(&TGAhead, sizeof(TGAhead), 1, out);
	fwrite(buffer, 3 * WINDOW_WIDTH*WINDOW_WIDTH, 1, out);
	fclose(out);
    }
    renderCount++;
    std::cout << renderCount << std::endl;

}

int main(int argc, char** argv)
{
    InitSPH();
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(WINDOW_WIDTH,WINDOW_WIDTH);
    glutCreateWindow("SPH");
    glutDisplayFunc(Render);
    glutIdleFunc(Update);
    InitGL();
    glutMainLoop();
    return 0;
}
