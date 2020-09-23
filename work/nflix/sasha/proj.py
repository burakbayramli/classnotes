from sasha import job
import numpy as np, sys, itertools
import random, re, sys

K = 7
    
class Proj(job.SashaJob):
        
    def __init__(self):
        job.SashaJob.__init__(self)
        self.randoms = {}
            
    def mapper(self, key, line):
        line_sps = re.findall("(\d+):(\d+)",line)
        result = np.zeros(K)
        for id,val in line_sps:
            j = int(id); val = float(val)
            if j not in self.randoms: 
                np.random.seed(j); self.randoms[j] = np.random.randn(K) 
            result += val * self.randoms[j]
        yield key, ";".join(map(lambda x: str(np.round(x,3)),result))
                    
if __name__ == "__main__":    
    Proj.run()
