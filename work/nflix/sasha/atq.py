from sasha import job
import numpy as np, sys, itertools
import random, re, sys, proj
import numpy.linalg as lin

'''
We calculate the matrix multiplication A transpose Q (AtQ) where A and
Q have m rows (large), n and k rows (large and small respectively).
'''
class AtQ(job.SashaJob):
    
    def __init__(self):
        job.SashaJob.__init__(self)
        self.mat_sum = np.zeros(proj.K) # for reducer
        self.dict = {}
        
    def mapper(self, id, line):
        [a,q] = line.split("|");
        left = re.findall("(\d+):(\d+)",a)
        right = np.array(map(np.float,q.split(';')))
        # iterate only non-zero elements in the bigger (left) vector
        for id,v in left:
            j = id; v = float(v)
            if j in self.dict:
                self.dict[j] += v*right
            else:
                self.dict[j] = v*right
        yield None, None
        
    def mapper_final(self):
        for j in self.dict.keys():
            out = ";".join(map(str,self.dict[j]))
            yield str(j), out
                        
    def reducer(self, val):
        val = np.array(map(np.float,val.split(';')))
        self.mat_sum += np.array(val)

    def result(self):
        yield ";".join(map(lambda x: str(np.round(x,3)),self.mat_sum))
        
if __name__ == "__main__":    
    AtQ.run()
