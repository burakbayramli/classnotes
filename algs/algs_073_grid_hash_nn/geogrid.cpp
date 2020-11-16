//
// g++ geogrid.cpp -g -O2 -o /tmp/a.exe; /tmp/a.exe
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

static int B = 200;
static int BIN_WIDTH = 10.f;
static int BIN_NUM = 20;
static int COORD_MAX = 200;

struct int3{
    int i, j, k;
    int3()
    {
	i=-1; j=-1; k=-1;
    }
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
    Particle() : x(0.f,0.f,0.f) {}
    Particle(float _x, float _y, float _z) : x(_x, _y, _z) {
	bin.i = calcBin(x[0]);
	bin.j = calcBin(x[1]);
	bin.k = calcBin(x[2]);
    }
    Vector3d x;
    int3 bin;
};

static vector<Particle> particles;

std::map<int3,  std::vector<Particle>> grid_hash; 

void InitSPH(void)
{
    for(int i = 0; i<B; i++) {
	float x = rand() % COORD_MAX;
	float y = rand() % COORD_MAX;
	float z = rand() % COORD_MAX;
	Particle p(x,y,z);

	std::cout
	    << i << " " 
	    << "[" << p.x.transpose() << "]"
	    <<  " in grid " 
	    << " " << p.bin.i
	    << " " << p.bin.j
	    << " " << p.bin.k
	    << std::endl;
	
	particles.push_back(p);

	int3 k(p.bin.i, p.bin.j, p.bin.k);
	grid_hash[k].push_back(p);
	
    }
    std::cout << particles.size() << std::endl;
}


std::vector<Particle> getNeighbors(Particle particle){

    std::vector<Particle> result;
    
    for (auto & i : {-1,0,1}) {
	for (auto & j : {-1,0,1}) {
	    for (auto & k : {-1,0,1}) {
		int ni = particle.bin.i + i;
		int nj = particle.bin.j + j;
		int nk = particle.bin.k + k;
		int3 newk(ni,nj,nk);
		std::cout << "neigh " << ni << " " << ni << " " << nk << " "
			  << grid_hash[newk].size() << std::endl;
	    }
	}
    }
    
    return result;
}


int main(int argc, char** argv)
{
    srand (0);
    
    InitSPH();

    /*
    int idx = 111;
    std::cout << "neighbors of " << particles[idx].x << std::endl;
    std::cout << "at " << particles[idx].bin.i << " " << particles[idx].bin.j << " " << particles[idx].bin.k << " "
	      << std::endl;
    std::vector<Particle> res = getNeighbors(particles[idx]);
    */

    for(auto &pi : particles)
    {
	for(auto &pj : particles)
	{
	    Vector3d rij = pj.x - pi.x;
	    float r2 = rij.norm();
	    std::cout << r2 << std::endl;
	}
	
    }

    
    
    return 0;
}
