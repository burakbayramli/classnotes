//
// g++ geogrid.cpp -g -Wall -O2 -o /tmp/a.exe; /tmp/a.exe
//
#include <map>
#include <iostream>
#include <fstream> 
#include <vector>
#include <cstdlib>
#include <unordered_map> 

using namespace std;

#include <eigen3/Eigen/Dense>
using namespace Eigen;

const static Vector3d G(0.f, 0.f, 0.f);

struct int3{
    int i, j, k;
};

struct Particle {
    Particle() : x(0.f,0.f,0.f) {}
    Particle(float _x, float _y, float _z) : x(_x, _y, _z) {}
    Vector3d x;
    //Vector3d bin;
    int3 bin;
};

static vector<Particle> particles;

static int B = 40;
static int BIN_WIDTH = 10.f;
static int BIN_NUM = 20;

static int COORD_MAX = 200;

static int bin(float x) {
    int res = (int)(x / BIN_WIDTH) + 1;
    if (res >= BIN_NUM) return BIN_NUM;
    return res;
}

//std::map<Vector3d,  std::vector<Particle>> m_bins; 
std::map<int3,  std::vector<Particle>> m_bins; 

void InitSPH(void)
{
    for(int i = 0; i<B; i++) {
	float x = rand() % COORD_MAX;
	float y = rand() % COORD_MAX;
	float z = rand() % COORD_MAX;
	Particle p(x,y,z);
	p.bin.i = bin(p.x[0]);
	p.bin.j = bin(p.x[1]);
	p.bin.k = bin(p.x[2]);
	std::cout        
	    << "[" << p.x.transpose() << "]"
	    << " " << p.bin.i << std::endl;
	particles.push_back(Particle(x,y,z));
    }
    std::cout << particles.size() << std::endl;
}



int main(int argc, char** argv)
{
    InitSPH();

    return 0;
}
