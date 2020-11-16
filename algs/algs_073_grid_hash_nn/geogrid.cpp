//
// g++ geogrid.cpp -g -Wall -O2 -o /tmp/a.exe; /tmp/a.exe
//
#include <iostream>
#include <fstream> 
#include <vector>
#include <cstdlib>
using namespace std;

#include <eigen3/Eigen/Dense>
using namespace Eigen;

const static Vector3d G(0.f, 0.f, 0.f); 

struct Particle {
    Particle(float _x, float _y, float _z) : x(_x, _y, _z) {}
    Vector3d x;
};

static vector<Particle> particles;

static int B = 40;

void InitSPH(void)
{
    for(int i = 0; i<B; i++) {
	float x = rand() % 200;
	float y = rand() % 200;
	float z = rand() % 200;
	std::cout << x << " " << y << " " << z<< std::endl;
	particles.push_back(Particle(x,y,z));
    }
    std::cout << particles.size() << std::endl;
}



int main(int argc, char** argv)
{
    InitSPH();

    return 0;
}
