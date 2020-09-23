'''
Logistic regression for map/reduce written for MRJob,
uses stochastic gradient descent.
'''
from mrjob.job import MRJob
from mrjob.protocol import PickleProtocol
import numpy as np
import os, thread

class MRLogisticRegression(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
    
    def __init__(self, *args, **kwargs):
        super(MRLogisticRegression, self).__init__(*args, **kwargs)
        self.n  = 1
        self.m = 3
        self.count = 0
        self.theta = np.ones((self.m,1))
        self.final_theta = np.zeros((self.m,self.n))

    def sigmoid(self, arr):
        return 1.0/(1+np.exp(-arr))

    def stoc_grad_ascent0(self, data_mat, label, theta):
        alpha = 0.01
        h = self.sigmoid(np.dot(data_mat,theta))
        theta = theta.T + (alpha * data_mat * (label - h))
        theta = theta.reshape((self.m,self.n))
        return theta
        
    def mapper(self, key, line):        
        tokens = map(np.float,line.split('\t'))
        data = np.append(1.0,np.array(tokens[:-1]))
        label = np.array(tokens[-1])
        self.theta = self.stoc_grad_ascent0(data, label, self.theta)
        
    def mapper_final(self):        
        yield ("key1", self.theta)
                
    def reducer(self, key, tokens):
        for val in tokens:
            self.final_theta += val
            self.count += 1
        yield('result',str(self.final_theta / self.count))
        
if __name__ == '__main__':
    MRLogisticRegression.run()
    
