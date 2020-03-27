import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin

N = 10
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)

@np.vectorize
def rbf1(x,y,m,gamma,w):
    return w*np.exp(-gamma*lin.norm(np.array([x,y])-m)  )

res = rbf1(1, 2, np.array([[3,3]]), gamma=0.1, w=0.5)
#print ('res',res)
zz = rbf1(xx, yy, m=2.0, gamma=0.1, w=0.5)
#print (zz.shape)
print
