// How to compile, and other info
// https://burakbayramli.github.io/dersblog/sk/2020/08/sph.html
// g++ simsph3.cpp  -std=c++1z -lX11 -lGL -lGLU -lglut -g -O2 -o /tmp/a.exe; /tmp/a.exe
// https://github.com/cerrno/mueller-sph
#include <map>
#include <GL/glut.h>
#include <iostream>
#include <fstream> 
#include <vector>
#include <iterator>
#include <eigen3/Eigen/Dense>

using namespace std;
using namespace Eigen;

// kordinate sistemi 0,500 arasinda, grafikleme icin -1,+1 ile
// surekli tercume yapiyoruz.

const static Vector3d G(0.f, 0.f, 1000*-9.8f); 
const static float REST_DENS = 1000.f; // rest density
const static float GAS_CONST = 2000.f; // const for equation of state
const static float H = 16.f; 
const static float DIST = 5.f;
const static float HSQ = H*H; 
const static float MASS = 65.f;
const static float VISC = 300.f;
const static float DT = 0.001f; 
static int BIN_WIDTH = 5.f;
static int BIN_NUM = 100;

// puruzsuzlestirici cekirdek ve turevleri
const static float POLY6 = 315.f/(65.f*M_PI*pow(H, 9.f));
const static float SPIKY_GRAD = -45.f/(M_PI*pow(H, 6.f));
const static float VISC_LAP = 45.f/(M_PI*pow(H, 6.f));

const static float EPS = H;
const static float BOUND_DAMPING = -0.5f;

static float MAX_COORD = 500;
static int WINDOW_WIDTH = 500;

struct int3 {

    int i, j, k;
    int3(){}
    int3(int _i, int _j, int _k)
    {
	i=_i; j=_j; k=_k;
    }    
    bool operator==(const int3 &rhs) const
    {
        return i == rhs.i && j == rhs.j && k == rhs.k;
    }        
    bool operator<(int3 const& rhs) const
    {	
	return i < rhs.i || ( i == rhs.i && j < rhs.j ) 
	    || ( i == rhs.i && j == rhs.j  && k < rhs.k ) ;
    }    
};

static int calcBin(float x) {
    int res = (int)(x / BIN_WIDTH) + 1;
    if (res >= BIN_NUM) return BIN_NUM;
    return res;
}

struct Particle {
    Particle() {
	//std::cout << "empty" << std::endl;
	//exit(0);
    }
    Particle(float _x, float _y, float _z, int _i) : x(_x, _y, _z),
				   v(0.f, 0.f, 0.f),
				   f(0.f, 0.f, 0.f),
				   rho(0.f),
				   p(0.f) {
	bin.i = calcBin(x[0]);
	bin.j = calcBin(x[1]);
	bin.k = calcBin(x[2]);
	i = _i;
    }
    Vector3d x, v, f;
    int3 bin;
    int i;
    float rho, p;
};

float glc(float x) {
    float res = x*(2.f/MAX_COORD) -1.f;
    return res;
}

static vector<Particle> particles;
static vector<Particle> glParticles;
std::map<int3,  std::vector<Particle>> grid_hash; 

std::map<int,  Particle>
getNeighbors(Particle particle){

    std::map<int,  Particle> result;

    // 3d izgara hucresinin etrafindaki tum hucrelere bak, bunlar
    // icinde oldugumuz dahil 27 tane, sag, sol, alt, ust, vs, tum
    // yonlere bakiyoruz
    for (auto & i : {-1,0,1}) {
	for (auto & j : {-1,0,1}) {
	    for (auto & k : {-1,0,1}) {
		int ni = particle.bin.i + i;
		int nj = particle.bin.j + j;
		int nk = particle.bin.k + k;
		int3 newk(ni,nj,nk);
		// bulduklarini sonuca ekle
		for (Particle & pn : grid_hash[newk]) {
		    result[pn.i] = pn;
		}
	    }
	}
    }
    
    return result;
}

void initGridHash(void)
{
    // sozlugu sil parcacik uzerindeki hucre indislerinin onceden
    // guncellenmis oldugunu farzet, o degerleri kullanip izgara
    // sozlugune her seyi tekrar ekle
    for(auto const& [key, value]: grid_hash)
    {
	grid_hash[key].clear();
    }
    for(auto &p : particles)
    {
	int3 k(p.bin.i, p.bin.j, p.bin.k);
	grid_hash[k].push_back(p);
    }
}


void InitSPH(void)
{
    int balls = 0;
    for(float x = 0.f; x < 100.f; x += 5.f) { 
	for(float y = 0.f; y < 500.f; y += 5.f) {	
	    for(float z = 0.f; z < 100.f; z += 5.f) {
		Particle p(x,y,z,balls);
		p.bin.i = calcBin(p.x[0]);
		p.bin.j = calcBin(p.x[1]);
		p.bin.k = calcBin(p.x[2]);	
		particles.push_back(p);
		glParticles.push_back(Particle(glc(x),glc(y),glc(z),balls));
		balls ++;
	    }
	}
    }
    
    initGridHash();

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

	p.bin.i = calcBin(p.x[0]);
	p.bin.j = calcBin(p.x[1]);
	p.bin.k = calcBin(p.x[2]);			
    }
}

void ComputeDensityPressure(void)
{
    for(auto &pi : particles)
    {
	pi.rho = 0.f;
	//for(auto &pj : particles)
	for(auto & [key, pj]: getNeighbors(pi)) 
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

	//std::cout <<  "neigh " << neigh.size() << std::endl;
	//for(auto &pj : particles)	    
	for(auto & [key, pj]: getNeighbors(pi)) 
        {	    
	    if(pi.i == pj.i) continue;

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
    initGridHash();
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
    glRotatef(200.f,0.0,1.0,0.0);
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
