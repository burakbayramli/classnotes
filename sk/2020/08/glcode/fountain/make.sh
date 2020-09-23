g++ -c Particle.cpp -o Particle.o -lX11 -lGL -lGLU -lglut -g -Wall -O2 
g++ -c V_Randomizer.cpp -o V_Randomizer.o -lX11 -lGL -lGLU -lglut -g -Wall -O2 
g++ main.cpp -g -Wall -O2 -o r.exe -lX11 -lGL -lGLU -lglut Particle.o V_Randomizer.o
