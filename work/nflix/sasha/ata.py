from sasha import job
import numpy as np, sys, itertools
from scipy import sparse
import random, re, sys, proj
import numpy.linalg as lin

class AtA(job.SashaJob):
    
    def __init__(self):
        job.SashaJob.__init__(self)
        self.buffer_size = 6
        self.data = [] # buffer for mapper
        self.row_sum = np.zeros(proj.K) # reducer 
            
    def mapper(self, key, line):
        line_vals = map(np.float,line.split(';'))
        self.data.append(line_vals)
        if len(self.data) == self.buffer_size:
            mult = np.dot(np.array(self.data).T,np.array(self.data))
            self.data = []
            for i, val in enumerate(mult):
                yield str(i), ";".join(map(lambda x: str(np.round(x,3)),val))

    def mapper_final(self):        
        if len(self.data) > 0:
            mult = np.dot(np.array(self.data).T,np.array(self.data)) 
            for i, val in enumerate(mult):
                yield str(i), ";".join(map(lambda x: str(np.round(x,3)),val))

    def reducer(self, row):
        self.row_sum += np.array(map(np.float,row.split(';')))
            
    def result(self):
        yield ";".join(map(lambda x: str(np.round(x,3)), self.row_sum))

                
if __name__ == "__main__":    
    AtA.run()
