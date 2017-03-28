from mrjob.job import MRJob
from mrjob.protocol import PickleProtocol
import numpy as np, sys

class MRAtA(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
    
    def __init__(self, *args, **kwargs):
        super(MRAtA, self).__init__(*args, **kwargs)
        self.buffer_size = 4
        self.n = 4
        self.data = []
        self.A_sum = np.zeros((self.n,self.n))
        
    def mapper(self, key, line):
        line_vals = map(np.float,line.split())
        self.data.append(line_vals)
        if len(self.data) == self.buffer_size:
            mult = np.dot(np.array(self.data).T,np.array(self.data))
            self.data = []
            for i, val in enumerate(mult):
                yield i, val        
    
    def reducer(self, i, tokens):
        for val in tokens:
            self.A_sum[i,:] += np.array(val)
        yield i, str(self.A_sum[i,:])

    '''
    At the end of processing a file, we might have some left over
    rows in self.data that were not multiplied because we did not
    reach buffer size. That condition is handled here. Whatever is
    left over, is simply multiplied and emitted.
    '''
    def mapper_final(self):        
        if len(self.data) > 0:
            mult = np.dot(np.array(self.data).T,np.array(self.data))
            for i, val in enumerate(mult):
                yield i, val        

    
if __name__ == '__main__':
    MRAtA.run()

