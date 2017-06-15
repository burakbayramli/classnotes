from mrjob.job import MRJob
from mrjob.protocol import PickleProtocol, RawValueProtocol
import numpy as np, sys
import random

class MRProj(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
    OUTPUT_PROTOCOL = RawValueProtocol
    
    def __init__(self, *args, **kwargs):
        super(MRProj, self).__init__(*args, **kwargs)
        self.k = 3
        
    def mapper(self, key, line):
        line_vals = map(np.float,line.split())
        result = np.zeros(self.k)
        for j,val in enumerate(line_vals):
            for i in range(self.k):
                random.seed(int(j + i))
                result[i] += val*random.gauss(0,1)
        yield (None,",".join(map(str,result)))
            
if __name__ == '__main__':
    MRProj.run()

