from numpy import *
from math import *
from numpy.linalg import *

rosen = lambda x: (1-x[0])**2 + 100*(x[1]-x[0]**2)**2

def rosen_d(x):
    return array([2*100*(x[1] - x[0]**2)*(-2*x[0]) - 2*(1.-x[0]), 2*100*(x[1]-x[0]**2)])

def BFGS(F,Fprime, Start, epsi = 10e-8, tol = 10e-6, sigma= 10**-1, beta = 10 ): 
    def LineSearch(g,x,s,sigma= 10**-1, beta = 10, convergval = 0.00001):
        def QFind(alpha):
            if abs(alpha) < convergval: return 1
            return (F(x + alpha*s) - F(x))/(alpha * dot(g,s))

        alpha = 1.

        while QFind(alpha) >= sigma:
            alpha=alpha * 2

        while QFind(alpha)< sigma:
            alphap=alpha / ( 2.0* ( 1- QFind(alpha)))
            alpha=max(1.0/beta * alpha, alphap)

        return alpha

    x = Start
    xold = inf
    N = shape(x)[0]
    H = 1.0 * eye(N)
    counter = 1
    alpha = 1
    g = Fprime(x)
    while norm(g) > epsi and norm(xold - x) > tol:
        s = -dot(H,g)
        alpha = LineSearch(g,x,s)
        x = x+alpha*s
        gold = g
        g = Fprime(x)
        y = (g - gold)/alpha
        dotsy = dot(s,y)
        if dotsy>0:
            z = dot(H,y)
            H += outer(s,s)*(dot(s,y) + dot(y, z))/dotsy**2 - \
                 (outer(z,s)+ outer(s, z)) / dotsy
        counter+=1
    return (x , counter)

(result, counter) =  BFGS(rosen, rosen_d, array([-1,0]))
print ("Custom BFGS:")
print ("Final Result: " + str(result))
print ("Iteration Count: " + str(counter))

