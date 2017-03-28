from mrjob.job import MRJob
from mrjob.protocol import PickleProtocol
import numpy as np, sys

class MRAB(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
    
    def __init__(self, *args, **kwargs):
        super(MRAB, self).__init__(*args, **kwargs)
        self.buffer_size = 4
        self.n = 4
        self.data = []
        # an example B
        self.B = np.array([[-20.49390153,   0.        ,   0.        ,   0.        ],
                           [-22.59208669,  -5.25334361,   0.        ,   0.        ],
                           [-12.88188096, -11.41585875,   4.44244436,   0.        ],
                           [-12.93067597, -12.53849977,   2.54158031,  -4.37310096]])

        
    def mapper(self, key, line):
        line_vals = map(np.float,line.split())
        self.data.append(line_vals)
        if len(self.data) == self.buffer_size:
            mult = np.dot(self.data,self.B.T)
            self.data = []
            yield (key, mult)
    
    def reducer(self, key, tokens):
        for x in tokens:
            yield (key, str(x))
    
if __name__ == '__main__':
    MRAB.run()

