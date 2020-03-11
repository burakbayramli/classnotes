import scipy.io as sio
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def subgrad_func(A,b,lam):
    n2 = A.shape[1]
    x = np.zeros((n2,1));

mat = sio.loadmat('A.mat')
A = mat['A']
mat = sio.loadmat('b.mat')
b = mat['b']
print (A.shape)
    
