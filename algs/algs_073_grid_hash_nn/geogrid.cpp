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
    int3() {} 
    int3(int _i, int _j, int _k) { i=_i; j=_j; k=_k;}
    int i, j, k;
};

struct int3compare
{
   bool operator() (const int3& lhs, const int3& rhs) const
   {
       return lhs.i < rhs.i && lhs.j < rhs.j && lhs.k < rhs.k;
   }
};

struct Particle {
    Particle() : x(0.f,0.f,0.f) {}
    Particle(float _x, float _y, float _z) : x(_x, _y, _z) {}
    Vector3d x;
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

std::map<int3,  std::vector<Particle>, int3compare> grid_hash; 

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

	int3 k(p.bin.i, p.bin.j, p.bin.k);
	grid_hash[k].push_back(p);
	
    }
    std::cout << particles.size() << std::endl;
}



int main(int argc, char** argv)
{

    InitSPH();
    
    return 0;
}
