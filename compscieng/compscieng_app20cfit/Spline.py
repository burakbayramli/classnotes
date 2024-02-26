import numpy as np

def Splines(data):
    np1=len(data)
    n=np1-1
    X,Y = zip(*data)
    X = [float(x) for x in X]
    Y = [float(y) for y in Y]
    a = Y[:]
    b = [0.0]*(n)
    d = [0.0]*(n)
    h = [X[i+1]-X[i] for i in xrange(n)]
    alpha = [0.0]*n
    for i in xrange(1,n):
        alpha[i] = 3/h[i]*(a[i+1]-a[i]) - 3/h[i-1]*(a[i]-a[i-1])
    c = [0.0]*np1
    L = [0.0]*np1
    u = [0.0]*np1
    z = [0.0]*np1
    L[0] = 1.0; u[0] = z[0] = 0.0
    for i in xrange(1,n):
        L[i] = 2*(X[i+1]-X[i-1]) - h[i-1]*u[i-1]
        u[i] = h[i]/L[i]
        z[i] = (alpha[i]-h[i-1]*z[i-1])/L[i]
    L[n] = 1.0; z[n] = c[n] = 0.0
    for j in xrange(n-1, -1, -1):
        c[j] = z[j] - u[j]*c[j+1]
        b[j] = (a[j+1]-a[j])/h[j] - (h[j]*(c[j+1]+2*c[j]))/3
        d[j] = (c[j+1]-c[j])/(3*h[j])
    splines = []
    for i in xrange(n):
        splines.append((a[i],b[i],c[i],d[i],X[i]))
    return splines,X[n]

def splinesToPlot(splines,xn,res):
    n=len(splines)
    perSpline = int(res/n)
    if perSpline < 3: perSpline = 3
    X=[]
    Y=[]
    for i in xrange(n-1):
        S = splines[i]
        x0 = S[4]
        x1 = splines[i+1][4]
        x = np.linspace(x0,x1,perSpline)
        for xi in x:
            X.append(xi)
            h=(xi-S[4])
            Y.append(S[0]+S[1]*h + S[2]*h**2 + S[3]*h**3)
    S=splines[n-1]
    x=np.linspace(S[4],xn,perSpline)
    for xi in x:
        X.append(xi)
        h=(xi-S[4])
        Y.append(S[0]+S[1]*h + S[2]*h**2 + S[3]*h**3)
    
    return X,Y
