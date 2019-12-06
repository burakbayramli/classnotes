import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def rosen(x):
    y = 100*(x[1]-x[0]**2)**2+(1-x[0])**2;

    gy =[-400*(x[1]-x[0]**2)*x[0]-2*(1-x[0]),
         200*(x[1]-x[0]**2)]

    return y,gy

x=np.array([-1.0,0])

H = np.eye(2)

tol = 1e-20

y,grad = rosen(x)

dist=2*tol
epsilon = tol

iter=0;

while lin.norm(grad)>1e-6
    print (1)
    break;


