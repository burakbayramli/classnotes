#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <sstream>
#include <string>
#include <fstream>
#include "segment-graph.h"

using namespace std;

int vertices_count = 0; int edges_count = 0;

universe* segment_distances(string file, float threshold, int min_size) {
  ifstream infile(file);  
  string line = "";
  int i=0; 
  edge* edges = NULL;
  while (getline(infile, line)) {
    if (line.find("%") != std::string::npos) continue;
    stringstream strstr(line);
    string word = "";
    int a; int b; float w;
    int j = 0;
    while (getline(strstr,word, ' ')) {
      if (j == 0) a = atoi(word.c_str());
      else if (j == 1) b = atoi(word.c_str());
      else if (j == 2) w = atof(word.c_str());
      j++;
    }    
    if (i == 0) {
      vertices_count = a;
      edges_count = (int)w;
      edges = new edge[edges_count];
    } else {
      edges[i-1].a = a-1;
      edges[i-1].b = b-1;
      edges[i-1].w = w;      
    }
    i++;
  }

  universe *u = segment_graph(vertices_count, edges_count, edges, threshold);
  
  // post process small components
  for (int i = 0; i < edges_count; i++) {
    int a = u->find(edges[i].a);
    int b = u->find(edges[i].b);
    if ((a != b) && ((u->size(a) < min_size) || (u->size(b) < min_size)))
      u->join(a, b);
  }
  
  return u;

}

int main(int argc, char **argv) {

  if( argc != 4){
      cout << "Usage: felzcluster file threshold min_size" << endl;
      return -1;
  }
  
  string file = argv[1];
  float threshold = atoi(argv[2]);
  int min_size = atoi(argv[3]);
  
  universe *u = segment_distances(file, threshold, min_size);
  
  cout << "point;cluster" << endl;
  for (int i=0;i<vertices_count;i++){
    cout << i << ";" << u->find(i) << endl;
  }
  
  return 0;
}

