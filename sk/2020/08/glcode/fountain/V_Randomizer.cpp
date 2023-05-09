#include <time.h>
#include <stdlib.h>
#include <math.h>
#include "V_Randomizer.h"

V_Randomizer::V_Randomizer(){
	seed = (unsigned)time(NULL);
	ran1idnum = -(long)seed; 
	qd2idnum = -(int)seed;
	srand(seed);
}

V_Randomizer::~V_Randomizer(){}

double V_Randomizer::rand(double low, double high){
	return (ran1()*(high - low))+low;
}

int V_Randomizer::rand(int low, int high){
	return (int) rand((double) low, (double) high);
}

#define IA 16807
#define IM 2147483647
#define AM (1.0/IM)
#define IQ 127773
#define IR 2836
#define NTAB 32
#define NDIV (1+(IM-1)/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)
double  V_Randomizer::ran1(){

	int j;
	long k;
	static long iy=0;
	static long iv[NTAB];
	float temp;
	if (ran1idnum <= 0 || !iy) {
		if (-(ran1idnum) < 1) ran1idnum=1;
		else ran1idnum = -(ran1idnum);
		for (j=NTAB+7;j>=0;j--) {
			k=(ran1idnum)/IQ;
			ran1idnum=IA*(ran1idnum-k*IQ)-IR*k;
			if (ran1idnum < 0) ran1idnum += IM;
			if (j < NTAB) iv[j] = ran1idnum;
		}
		iy=iv[0];
	}
	k=(ran1idnum)/IQ;
	ran1idnum=IA*(ran1idnum-k*IQ)-IR*k;
	if (ran1idnum < 0) ran1idnum += IM;
	j=iy/NDIV;
	iy=iv[j];
	iv[j] = ran1idnum;
	temp=(float)AM*iy;
	if ( temp > RNMX){ 
		return RNMX;
	}else{
		return temp;
	}
}
#undef IA
#undef IM
#undef AM
#undef IQ
#undef IR
#undef NTAB
#undef NDIV
#undef EPS
#undef RNMX



/* mean m, standard deviation s */
double V_Randomizer::box_muller(double m, double s)	{				        
	int idnum = -1;
	double x1, x2, w, y1;
	static double y2;
	static int use_last = 0;

	if (use_last){/* use value from previous call */
		y1 = y2;
		use_last = 0;
	}else{
		do{
			x1 = 2.0 * ran1() - 1.0;
			x2 = 2.0 * ran1() - 1.0;
			w = x1 * x1 + x2 * x2;
		}while ( w >= 1.0 );

		w = sqrt((-2.0 *log(w))/ w);
		y1 = x1 * w;
		y2 = x2 * w;
		use_last = 1;
	}

	return( m + y1 * s );
}

