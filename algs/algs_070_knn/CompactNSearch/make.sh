g++  -c CompactNSearch.cpp -o /tmp/CompactNSearch.o  -g -Wall -O2 
g++ main.cpp -g -Wall -O2 -o /tmp/a.exe  -fopenmp -lstdc++ -lm /tmp/CompactNSearch.o 
