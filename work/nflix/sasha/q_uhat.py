from sasha import job
import numpy as np, sys, itertools
from scipy import sparse
import random, re, sys
import numpy.linalg as lin

class QUHat(job.SashaJob):
    
    def __init__(self):
        job.SashaJob.__init__(self)
        U,S,VT = lin.svd(np.loadtxt("R_BT.dat",delimiter=';'))
        self.Uhat = VT.T  # because B=USV', B'=VSU' for U of B we need V'
        self.data = []
        self.buffer_size = 4
            
    def mapper(self, key, line):
        line = map(np.float,line.split(';'))
        self.data.append(line)
        if len(self.data) == self.buffer_size:
            mult = np.dot(self.data,self.Uhat)
            self.data = []
            for row in mult:
                yield key, ";".join(map(lambda x: str(np.round(x,3)),row))
        
if __name__ == "__main__":    
    QUHat.run()
