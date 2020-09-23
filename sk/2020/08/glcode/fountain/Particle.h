class Particle {
	typedef struct _Vector{
		double x,y,z;
} Vector;
protected: 
	Vector pos, vel, acc;
	float age;
	float alpha;
	float colour[3];
	int partType;
	float size;
public:
	Particle();
	~Particle();
void setParticleType(int a);
void Update(double time);
void Render();
void setSize(float newSize);
void setColour(float r,float g, float b);
Particle(double _posX, double _posY, double _posZ, double _velX, double _velY, double _velZ, float colR, float colG, float colB, float colA);
};
