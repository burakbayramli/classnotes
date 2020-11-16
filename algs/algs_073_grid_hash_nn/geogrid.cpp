// How to compile, and other info
// https://burakbayramli.github.io/dersblog/sk/2020/08/sph.html
// g++ geogrid.cpp -g -Wall -O2 -o /tmp/a.exe; /tmp/a.exe
// https://github.com/cerrno/mueller-sph
#include <iostream>
#include <fstream> 
#include <vector>
#include <cstdlib>
using namespace std;

#include <eigen3/Eigen/Dense>
using namespace Eigen;

// "Particle-Based Fluid Simulation for Interactive Applications"
// solver parameters
const static Vector3d G(0.f, 0.f, 5.0f*-9.8f); // external (gravitational) forces

// particle data structure
// stores position, velocity, and force for integration
// stores density (rho) and pressure values for SPH
struct Particle {
    Particle(float _x, float _y, float _z) : x(_x, _y, _z) {}
    Vector3d x;
};

// solver data
static vector<Particle> particles;
static int B = 40;

void InitSPH(void)
{
    for(int i = 0; i<B; i++) {
	float x = rand() % 200;
	float y = rand() % 200;
	float z = rand() % 200;
	particles.push_back(Particle(x,y,z));
    }
    std::cout << particles.size() << std::endl;
}



int main(int argc, char** argv)
{
    InitSPH();

    return 0;
}
