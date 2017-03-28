#g++ -c -O3 -Wall `pkg-config --cflags opencv` -D LINUX -fPIC videostab.cpp
#g++ -o videostab.exe videostab.o `pkg-config --libs opencv`
g++ -c -O3 -Wall `pkg-config --cflags opencv` -D LINUX -fPIC vs.cpp
g++ -o vs.exe vs.o `pkg-config --libs opencv`
