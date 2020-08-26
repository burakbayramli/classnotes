#pragma once
class V_Randomizer{
private:
	 int seed;
	 long ran1idnum;
	 int qd2idnum;
public:
	V_Randomizer();
	~V_Randomizer();
	double rand(double low, double high);
	int rand(int low, int high);
	double ran1();
	double box_muller(double m, double s);

	inline double ranqd2(){
		unsigned long itemp;
		static unsigned long jflone = 0x3f800000;
		static unsigned long jflmsk = 0x007fffff;
		qd2idnum = 1664525L * qd2idnum + 1013904223L;
		itemp = jflone | (jflmsk & qd2idnum);
		return (double)(*(float *)&itemp)-1.0;
	}
	
};
