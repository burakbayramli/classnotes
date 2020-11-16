// 
// Geometrik izgara yontemi ile komsu kordinatlari bulmak
// Keywords: Grid based nearest neighbor finding within a radius
//
// Derlemek icin: g++ geogrid.cpp -g -O2 -o /tmp/a.exe; /tmp/a.exe
// 
//
#include <map>
#include <iostream>
#include <vector>
#include <cstdlib>

using namespace std;

#include <eigen3/Eigen/Dense>
using namespace Eigen;

static int B = 200;
static int BIN_WIDTH = 20.f;
static int BIN_NUM = 10;
static int COORD_MAX = 200;


// bir izgara hucresini temsil eden uc tane tam sayi bu struct icinde
struct int3 {

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

// parcaciklar bu sinifta, her parcacik indisini, hangi izgara
// hucresinde oldugunu, ve kordinatlarini bilir
struct Particle {
    Particle() : x(0.f,0.f,0.f) {}
    Particle(float _x, float _y, float _z, int _i) : x(_x, _y, _z) {
	bin.i = calcBin(x[0]);
	bin.j = calcBin(x[1]);
	bin.k = calcBin(x[2]);
	i = _i;
    }
    Vector3d x;
    int3 bin;
    int i;
};


// tum parcaciklar
static vector<Particle> particles;

// her izgara hucresindeki parcaciklari tuttugumuz yer
std::map<int3,  std::vector<Particle>> grid_hash; 

void Init(void)
{
    for(int i = 0; i<B; i++) {
	float x = rand() % COORD_MAX;
	float y = rand() % COORD_MAX;
	float z = rand() % COORD_MAX;
	Particle p(x,y,z,i);

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


// bir parcacigin tum komsularini bul, anahtari parcacik indisi degeri
// parcacik objesi olan bir esleme / sozluk icinde donduruyoruz ki
// boylece "bu komsu var mi yok mu?" sorusu hizla sorulabiliyor
// (anahtar varligini kontrol cok hizlidir)
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
		std::vector<Particle> grid_particles = grid_hash[newk];
		for (Particle & pn : grid_particles) {
		    result[pn.i] = pn;
		}
	    }
	}
    }
    
    return result;
}


int main(int argc, char** argv)
{
    srand (0);
    
    Init();

    int tp = 0; int tn = 0; int fp = 0; int fn = 0;
    for(auto &pi : particles)
    {
	for(auto &pj : particles)
	{
	    std::map<int,  Particle> res = getNeighbors(pi);
	    
	    Vector3d rij = pj.x - pi.x;
	    float d = rij.squaredNorm();
	    
	    if (res.count(pj.i) == 1 && d <= BIN_WIDTH) {
		tp++;
	    }
	    else if (res.count(pi.i) != 1 && d > BIN_WIDTH) {
		tn++;
	    }
	    else if (res.count(pi.i) == 1 && d > BIN_WIDTH) {
		fp++;
	    }
	    else if (res.count(pi.i) != 1 && d <= BIN_WIDTH) {
		fn++;
	    }
	    
	}
	
    }

    std::cout << "tp:" << tp << std::endl;
    std::cout << "tn:" << tn << std::endl;
    std::cout << "fp:" << fp << std::endl;
    std::cout << "fn:" << fn << std::endl;
    
    return 0;
}
