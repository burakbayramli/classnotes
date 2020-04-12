from sasha import job
import numpy.linalg as lin
import numpy as np, sys, itertools
from scipy import sparse
import random, re, sys

class AInvR(job.SashaJob):
    
    def __init__(self):
        job.SashaJob.__init__(self)
        self.R_inv = lin.inv(np.loadtxt("R.dat",delimiter=';'))
            
    def mapper(self, key, line):
        line_vals = map(np.float,line.split(';'))
        mult = np.dot(line_vals,self.R_inv)
        yield key, ";".join(map(lambda x: str(np.round(x,3)),mult))
        
if __name__ == "__main__":    
    AInvR.run()
