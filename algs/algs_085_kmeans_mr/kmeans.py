from mrjob.job import MRJob
from mrjob.protocol import PickleProtocol
import numpy as np, sys
import pandas as pd
import os, random

def euc_to_clusters(x,y):
    return np.sqrt(np.sum((x-y)**2, axis=1))

class MRKMeans(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
    
    def __init__(self, *args, **kwargs):
        super(MRKMeans, self).__init__(*args, **kwargs)
        self.centers_ = pd.read_csv("/tmp/centers.csv",header=None,sep="   ")
        self.k = 15
        
    def mapper(self, key, line):
        point = np.array(map(np.float,line.split('   ')))
        c = np.argmin(euc_to_clusters(np.array(self.centers_), point))
        yield(c, point)
                        
    def reducer(self, key, tokens):
        new_centers = np.zeros((1,2))
        counts = 0
        for val in tokens:
            new_centers += val
            counts += 1
        yield('final', (key, new_centers[0] / counts))
        
    def reduce_all_centers(self, key, values):
        new_centers = np.zeros((self.k,2))
        self.f=open("/tmp/centers.csv","w")
        for (cluster,val) in values:
            print cluster, val
            new_centers[cluster] = val
        for row in new_centers:
            self.f.write("   ".join(map(str,row)))
            self.f.write("\n")
        self.f.close()
        
    def steps(self):
        return [self.mr(mapper=self.mapper,reducer=self.reducer),
                self.mr(reducer=self.reduce_all_centers)]
    
if __name__ == '__main__':
    for i in range(15): MRKMeans.run()

