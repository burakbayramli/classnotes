
// OpenGL Graphics includes
#include <helper_gl.h>
#include <GL/freeglut.h>
#include <cuda_runtime.h>
#include <helper_cuda.h>    // includes cuda.h and cuda_runtime_api.h

#include <stdlib.h>
#include <cstdlib>
#include <cstdio>
#include <algorithm>

#include "particleSystem.h"

#define GRID_SIZE       64
#define NUM_PARTICLES   16384

uint numParticles = 0;

uint3 gridSize;

float timestep = 0.5f;
float damping = 1.0f;
float gravity = 0.0003f;

float collideSpring = 0.5f;;
float collideDamping = 0.02f;;
float collideShear = 0.1f;
float collideAttraction = 0.0f;

ParticleSystem *psystem = 0;

int currentFrame = 0;

extern "C" void cudaInit(int argc, char **argv);
extern "C" void cudaGLInit(int argc, char **argv);
extern "C" void copyArrayFromDevice(void *host, const void *device, unsigned int vbo, int size);

void initParticleSystem(int numParticles, uint3 gridSize)
{
    psystem = new ParticleSystem(numParticles, gridSize);
    psystem->reset(ParticleSystem::CONFIG_GRID);

}

void initGL(int *argc, char **argv)
{
    glutInit(argc, argv);
    glutCreateWindow("CUDA Particles");
}

void step()
{
    psystem->setDamping(damping);
    psystem->setGravity(-gravity);
    psystem->setCollideSpring(collideSpring);
    psystem->setCollideDamping(collideDamping);
    psystem->setCollideShear(collideShear);
    psystem->setCollideAttraction(collideAttraction);
    psystem->update(timestep);
    std::cout << currentFrame << std::endl;
    if (currentFrame % 10 == 0) 
	psystem->dumpParticles();
    currentFrame++;
}

int
main(int argc, char **argv)
{
#if defined(__linux__)
    setenv ("DISPLAY", ":0", 0);
#endif

    numParticles = NUM_PARTICLES;
    uint gridDim = GRID_SIZE;

    gridSize.x = gridSize.y = gridSize.z = gridDim;
    printf("grid: %d x %d x %d = %d cells\n",
	   gridSize.x, gridSize.y, gridSize.z,
	   gridSize.x*gridSize.y*gridSize.z);
    printf("particles: %d\n", numParticles);

    initGL(&argc, argv);

    cudaInit(argc, argv);

    initParticleSystem(numParticles, gridSize);

    for (int i=0;i<800;i++){
	step();
    }

    exit(0);
}

