import sys
from math  import *

class wave :
    def __init__ (self, data):
        self.data = data
    def __init__ (self, points=2387, formula=None) :
	self.data = [0.0]*points
	self.points= points
	if formula :
	    for p in range(points) :
		x = p*pi*2/points
		self.data[p] = eval(formula)
                
    def __add__ (self, other) :
	target = wave(points=self.points)
	for i in range(self.points) :
	    target.data[i] = self.data[i] + other.data[i]
	return target

    def __mul__ (self, other) :
        target = wave(points=self.points)
        if type(other) == type(5) or type(other) == type(5.0) :
            for i in range(self.points) :
                target.data[i] = self.data[i] * other
        else :
            for i in range(self.points) :
                #print target.points, self.points, other.points
                target.data[i] = self.data[i] * other.data[i]
        return target

    def __sub__ (self, other) :
        target = wave(points=self.points)
        for i in range(self.points) :
            target.data[i] = self.data[i] - other.data[i]
        return target

    def integral(self) :
        ans = 0.0
        for pt in self.data : ans = ans+pt
        return ans*2*pi/self.points

    def fft (self) :
        work = self * 1   # Harmonics will be stripped from this
        for harm in range(1,10) :
            formula="-sin(%d*x)" % harm
            area = (wave(formula=formula)*work).integral()
            amplitude = area/-pi
            if amplitude > .000001 :
                print "Harmonic=",harm, " Amplitude=%.04f" % amplitude
            takeAway = wave(formula="sin(%d*x) * %f" % (harm,amplitude))
            work = work-takeAway

if __name__ == "__main__": 

    p1 = wave(formula="sin(x)/1")
    p3 = wave(formula="sin(3*x)/3")
    p5 = wave(formula="sin(5*x)/5")
    mys = p1+p3+p5
    print mys.fft()
    print mys.points
    print len(mys.data)
