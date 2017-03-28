from sasha import job
import numpy as np, sys, itertools
from scipy import sparse
import random, re, sys, proj
import numpy.linalg as lin

class Join(job.SashaJob):
    
    def __init__(self):
        job.SashaJob.__init__(self)
        self.left = None
        self.right = None
        
    def reducer(self, line):
        if ':' in line: self.left = line
        else: self.right = line
        if self.left and self.right: is_complete = True
            
    def result(self):
        if self.left and self.right:
            yield self.left + "|" + self.right
        else: 
            yield None
                
if __name__ == "__main__":    
    Join.run()
