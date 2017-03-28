'''
Read sasha output and run SVD on the original data, overlay plots
'''
import numpy as np
import numpy.random as rand
import numpy.linalg as lin
import matplotlib.pyplot as plt
import pandas as pd

#fin = open("/home/burak/Downloads/sasha/node1/U_final.dat")
fin = open("/tmp/U_final.dat")
arr = []
for x in fin.readlines():
    [id,row] = x.strip().split('\t')
    arr.append(map(np.float,row.split(';')))
    
U = np.array(arr)
plt.plot(-U[:,0],U[:,1],'r+')
plt.hold(True)
        
df = pd.read_csv("../w1.csv",sep=';')
A = np.array(df)[:,1:]
U, Sigma, V = lin.svd(A);
plt.plot(U[:,0],U[:,1],'bx')
plt.show()
