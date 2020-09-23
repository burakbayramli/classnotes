#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory.h>
#include <GL/glut.h>
#include <iostream> 
#include <fstream> 
using namespace std;
 
#define kScreenWidth 640
#define kScreenHeight 480
#define kViewWidth 10.0f
#define kViewHeight (kScreenHeight*kViewWidth/kScreenWidth)
#define kPi 3.1415926535f
#define kParticleCount 3000
 
#define kRestDensity 82.0f
#define kStiffness 0.08f
#define kNearStiffness 0.1f
#define kSurfaceTension 0.0004f
#define kLinearViscocity 0.5f
#define kQuadraticViscocity 1.0f
 
#define kParticleRadius 0.05f
#define kH (6*kParticleRadius)
#define kFrameRate 20
#define kSubSteps 7
 
#define kDt ((1.0f/kFrameRate) / kSubSteps)
#define kDt2 (kDt*kDt)
#define kNorm (20/(2*kPi*kH*kH))
#define kNearNorm (30/(2*kPi*kH*kH))
 
 
#define kEpsilon 0.0000001f
#define kEpsilon2 (kEpsilon*kEpsilon)
 
int renderCount = 0;

struct Particle
{
    float x;
    float y;
 
    float u;
    float v;
 
    float P;
    float nearP;
 
    float m;
 
    float density;
    float nearDensity;
    Particle* next;
};
 
struct Vector2
{
    Vector2() { }
    Vector2(float x, float y) : x(x) , y(y) { }
    float x;
    float y;
};
 
struct Wall
{
    Wall() { }
    Wall(float _nx, float _ny, float _c) : nx(_nx), ny(_ny), c(_c) { }
    float nx;
    float ny;
    float c;
};
 
struct Rgba
{
    Rgba() { }
    Rgba(float r, float g, float b, float a) : r(r), g(g), b(b), a(a) { }
    float r, g, b, a;
};
 
struct Material
{
    Material() { }
    Material(const Rgba& colour,
	     float mass,
	     float scale,
	     float bias) : colour(colour) ,
			   mass(mass) ,
			   scale(scale) ,
			   bias(bias) { }
    Rgba colour;
    float mass;
    float scale;
    float bias;
};
 
#define kMaxNeighbourCount 64
struct Neighbours
{
    const Particle* particles[kMaxNeighbourCount];
    float r[kMaxNeighbourCount];
    size_t count;
};
 
size_t particleCount = 0;
Particle particles[kParticleCount];
Neighbours neighbours[kParticleCount];
Vector2 prevPos[kParticleCount];
Vector2 relaxedPos[kParticleCount];
Material particleMaterials[kParticleCount];
Rgba shadedParticleColours[kParticleCount];
 
#define kWallCount 4
Wall walls[kWallCount] =
    {
	Wall( 1,  0, 0),
	Wall( 0,  1, 0),
	Wall(-1,  0, -kViewWidth),
	Wall( 0, -1, -kViewHeight)
    };
 
#define kCellSize kH
const size_t kGridWidth = (size_t)(kViewWidth / kCellSize);
const size_t kGridHeight = (size_t)(kViewHeight / kCellSize);
const size_t kGridCellCount = kGridWidth * kGridHeight;
Particle* grid[kGridCellCount];
size_t gridCoords[kParticleCount*2];
 
 
struct Emitter
{
    Emitter(const Material& material, const Vector2& position,
	    const Vector2& direction, float size,
	    float speed, float delay) : material(material),
					position(position),
					direction(direction),
					size(size),
					speed(speed),
					delay(delay),
					count(0)
    {
	float len = sqrt(direction.x*direction.x + direction.y*direction.y);
	this->direction.x /= len;
	this->direction.y /= len;
    }
    Material material;
    Vector2 position;
    Vector2 direction;
    float size;
    float speed;
    float delay;
    size_t count;
};
 
#define kEmitterCount 2
Emitter emitters[kEmitterCount] =
    {
	Emitter(
		Material(Rgba(0.6f, 0.7f, 0.9f, 1), 1.0f, 0.08f, 0.9f),
		Vector2(0.05f*kViewWidth, 0.8f*kViewHeight), Vector2(4, 1), 0.2f, 5, 0),
	Emitter(
		Material(Rgba(0.1f, 0.05f, 0.3f, 1), 1.4f, 0.075f, 1.5f),
		Vector2(0.05f*kViewWidth, 0.9f*kViewHeight), Vector2(4, 1), 0.2f, 5, 6),
    };
 
 
float Random01() { return (float)rand() / (float)(RAND_MAX-1); }
float Random(float a, float b) { return a + (b-a)*Random01(); }
 
 
void UpdateGrid()
{
    // Clear grid
    memset(grid, 0, kGridCellCount*sizeof(Particle*));
 
    // Add particles to grid
    for (size_t i=0; i<particleCount; ++i)
    {
	Particle& pi = particles[i];
	int x = pi.x / kCellSize;
	int y = pi.y / kCellSize;
 
	if (x < 1)
	    x = 1;
	else if (x > kGridWidth-2)
	    x = kGridWidth-2;
 
	if (y < 1)
	    y = 1;
	else if (y > kGridHeight-2)
	    y = kGridHeight-2;
 
	pi.next = grid[x+y*kGridWidth];
	grid[x+y*kGridWidth] = &pi;
 
	gridCoords[i*2] = x;
	gridCoords[i*2+1] = y;
    }
}
 
 
void ApplyBodyForces()
{
    for (size_t i=0; i<particleCount; ++i)
    {
	Particle& pi = particles[i];
	pi.v -= 9.8f*kDt;
	//cout << pi.v << endl << endl;
    }
}
 
 
void Advance()
{
    for (size_t i=0; i<particleCount; ++i)
    {
	Particle& pi = particles[i];
 
	// preserve current position
	prevPos[i].x = pi.x;
	prevPos[i].y = pi.y;
 
	pi.x += kDt * pi.u;
	pi.y += kDt * pi.v;
    }
}
 
 
void CalculatePressure()
{
    for (size_t i=0; i<particleCount; ++i)
    {
	Particle& pi = particles[i];
	size_t gi = gridCoords[i*2];
	size_t gj = gridCoords[i*2+1]*kGridWidth;
 
	neighbours[i].count = 0;
 
	float density = 0;
	float nearDensity = 0;
	for (size_t ni=gi-1; ni<=gi+1; ++ni)
        {
	    for (size_t nj=gj-kGridWidth; nj<=gj+kGridWidth; nj+=kGridWidth)
            {
		for (Particle* ppj=grid[ni+nj]; NULL!=ppj; ppj=ppj->next)
                {
		    const Particle& pj = *ppj;
 
		    float dx = pj.x - pi.x;
		    float dy = pj.y - pi.y;
		    float r2 = dx*dx + dy*dy;
		    if (r2 < kEpsilon2 || r2 > kH*kH)
			continue;
 
		    float r = sqrt(r2);
		    float a = 1 - r/kH;
		    density += pj.m * a*a*a * kNorm;
		    nearDensity += pj.m * a*a*a*a * kNearNorm;
 
		    if (neighbours[i].count < kMaxNeighbourCount)
                    {
			neighbours[i].particles[neighbours[i].count] = &pj;
			neighbours[i].r[neighbours[i].count] = r;
			++neighbours[i].count;
                    }
                }
            }
        }
 
	pi.density = density;
	pi.nearDensity = nearDensity;
	pi.P = kStiffness * (density - pi.m*kRestDensity);
	pi.nearP = kNearStiffness * nearDensity;
    }
}
 
 
void CalculateRelaxedPositions()
{
    for (size_t i=0; i<particleCount; ++i)
    {
	const Particle& pi = particles[i];
 
	float x = pi.x;
	float y = pi.y;
 
	for (size_t j=0; j<neighbours[i].count; ++j)
        {
	    const Particle& pj = *neighbours[i].particles[j];
	    float r = neighbours[i].r[j];
	    float dx = pj.x - pi.x;
	    float dy = pj.y - pi.y;
 
	    float a = 1 - r/kH;
 
	    float d = kDt2 * ((pi.nearP+pj.nearP)*a*a*a*kNearNorm +
			      (pi.P+pj.P)*a*a*kNorm) / 2;
 
	    // relax
	    x -= d * dx / (r*pi.m);
	    y -= d * dy / (r*pi.m);
 
	    // surface tension
	    if (pi.m == pj.m)
            {
		x += (kSurfaceTension/pi.m) * pj.m*a*a*kNorm * dx;
		y += (kSurfaceTension/pi.m) * pj.m*a*a*kNorm * dy;
            }
 
	    // viscocity
	    float du = pi.u - pj.u;
	    float dv = pi.v - pj.v;
	    float u = du*dx + dv*dy;
	    if (u > 0)
            {
		u /= r;
 
		float a = 1 - r/kH;
		float I = 0.5f * kDt * a * (kLinearViscocity*u + kQuadraticViscocity*u*u);
 
		x -= I * dx * kDt;
		y -= I * dy * kDt;
            }
 
        }
 
	relaxedPos[i].x = x;
	relaxedPos[i].y = y;
    }
}
 
 
void MoveToRelaxedPositions()
{
    for (size_t i=0; i<particleCount; ++i)
    {
	Particle& pi = particles[i];
	pi.x = relaxedPos[i].x;
	pi.y = relaxedPos[i].y;
	pi.u = (pi.x - prevPos[i].x) / kDt;
	pi.v = (pi.y - prevPos[i].y) / kDt;
    }
}
 
 
void ResolveCollisions()
{
    for (size_t i=0; i<particleCount; ++i)
    {
	Particle& pi = particles[i];
 
	for (size_t j=0; j<kWallCount; ++j)
        {
	    const Wall& wall = walls[j];
	    float dis = wall.nx*pi.x + wall.ny*pi.y - wall.c;
	    if (dis < kParticleRadius)
            {
		//float d = pi.u*wall.nx + pi.v*wall.ny;
		if (dis < 0)
		    dis = 0;
		pi.u += (kParticleRadius - dis) * wall.nx / kDt;
		pi.v += (kParticleRadius - dis) * wall.ny / kDt;
            }
        }
    }
}
 
 
void Render()
{    
    glClearColor(0.02f, 0.01f, 0.01f, 1);
    glClear(GL_COLOR_BUFFER_BIT);
 
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, kViewWidth, 0, kViewHeight, 0, 1);
 
    glEnable(GL_POINT_SMOOTH);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 
    for (size_t i=0; i<particleCount; ++i)
    {
	const Particle& pi = particles[i];
	const Material& material = particleMaterials[i];
 
	Rgba& rgba = shadedParticleColours[i];
	rgba = material.colour;
	rgba.r *= material.bias + material.scale*pi.P;
	rgba.g *= material.bias + material.scale*pi.P;
	rgba.b *= material.bias + material.scale*pi.P;
    }
 
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
 
    glPointSize(2.5f*kParticleRadius*kScreenWidth/kViewWidth);
 
    glColorPointer(4, GL_FLOAT, sizeof(Rgba), shadedParticleColours);
    glVertexPointer(2, GL_FLOAT, sizeof(Particle), particles);
    glDrawArrays(GL_POINTS, 0, particleCount);
 
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
 
    glutSwapBuffers();

    // her 40'inci goruntuyu diske yazmak
    if (renderCount % 40 == 0) {
	int* buffer = new int[ kScreenWidth * kScreenHeight * 3 ];
	glReadPixels( 0, 0, kScreenWidth, kScreenHeight, GL_BGR, GL_UNSIGNED_BYTE, buffer );
	std::string fname = "/tmp/gl1-out-" + std::to_string(renderCount) + ".tga";
	//std::string fname = "/tmp/gl1-out-1.tga";
	FILE   *out = fopen(fname.c_str(), "w");
	short  TGAhead[] = {0, 2, 0, 0, 0, 0, kScreenWidth, kScreenHeight, 24};
	fwrite(&TGAhead, sizeof(TGAhead), 1, out);
	fwrite(buffer, 3 * kScreenWidth*kScreenHeight, 1, out);
	fclose(out);
    }
    renderCount++;
}
 
 
void EmitParticles()
{
    if (particleCount == kParticleCount)
	return;
 
    static int emitDelay = 0;
    if (++emitDelay < 3)
	return;
 
    for (size_t emitterIdx=0; emitterIdx<kEmitterCount; ++emitterIdx)
    {
	Emitter& emitter = emitters[emitterIdx];
	if (emitter.count >= kParticleCount/kEmitterCount)
	    continue;
 
	emitter.delay -= kDt*emitDelay;
	if (emitter.delay > 0)
	    continue;
 
	size_t steps = emitter.size / (2*kParticleRadius);
 
	for (size_t i=0; i<=steps && particleCount<kParticleCount; ++i)
        {
	    Particle& pi = particles[particleCount];
	    Material& material = particleMaterials[particleCount];
	    ++particleCount;
 
	    ++emitter.count;
 
	    float ofs = (float)i / (float)steps - 0.5f;
 
	    ofs *= emitter.size;
	    pi.x = emitter.position.x - ofs*emitter.direction.y;
	    pi.y = emitter.position.y + ofs*emitter.direction.x;
	    pi.u = emitter.speed * emitter.direction.x*Random(0.9f, 1.1f);
	    pi.v = emitter.speed * emitter.direction.y*Random(0.9f, 1.1f);
	    pi.m = emitter.material.mass;
 
	    material = emitter.material;
        }
    }
 
    emitDelay = 0;
}
 
 
void Update()
{
    for (size_t step=0; step<kSubSteps; ++step)
    {
	EmitParticles();
 
	ApplyBodyForces();
	Advance();
	UpdateGrid();
	CalculatePressure();
	CalculateRelaxedPositions();
	MoveToRelaxedPositions();
	UpdateGrid();
	ResolveCollisions();
    }
 
    glutPostRedisplay();
}
 
 
int main (int argc, char** argv)
{
    glutInitWindowSize(kScreenWidth, kScreenHeight);
    glutInit(&argc, argv);
    glutInitDisplayString("samples stencil>=3 rgb double depth");
    glutCreateWindow("SPH");
    glutDisplayFunc(Render);
    glutIdleFunc(Update);
     
    memset(particles, 0, kParticleCount*sizeof(Particle));
    UpdateGrid();
 
    glutMainLoop();
     
    return 0;
}
      
