import numpy as np
import matplotlib.pyplot as plt

def f(t,A,k,K):
    return K / (1+A*np.exp(-k*t))    

k = 1
K = 2
A = 30

t = np.linspace(0,10,100)
plt.plot(t, f(t,A,k,K))
plt.show()
