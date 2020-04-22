INCDIR = -I.
DBG    = -g
OPT    = -O3
CPP    = g++  -std=c++11 
CFLAGS = $(DBG) $(OPT) $(INCDIR) 
LINK   = -lm 

.cpp.o:
	$(CPP) $(CFLAGS) -c $< -o $@

all: felzclust

felzclust: felzclust.cpp  segment-graph.h disjoint-set.h
	$(CPP) $(CFLAGS) -o felzclust felzclust.cpp $(LINK)

clean:
	/bin/rm -f felzclust *.o

clean-all: clean
	/bin/rm -f *~ 

