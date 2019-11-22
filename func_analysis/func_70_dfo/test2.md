
```python
g = np.array([-502.20585098, -175.47731298])
g2 = np.array([-404., -200.])
hess = np.array([[1004.41170195,  350.95462595],[ 350.95462595,  170.56735242]])
hess2 = np.array([[1202.,  400.],[ 400.,  200.]])

newton_dir = -np.dot(lin.inv(hess),g)
print ('newton dir',newton_dir)
newton_dir2 = -np.dot(lin.inv(hess2),g2)
print ('newton dir2',newton_dir2)

```

```text
newton dir [5.0000000e-01 6.7855499e-11]
newton dir2 [0.00995025 0.9800995 ]
```

```python
def rosenbrock(x):
    return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

def Rosenbrock(x,y):
    return (1 - x)**2 + 100*(y - x**2)**2

def Grad_Rosenbrock(x):
    g1 = -400*x[0]*x[1] + 400*x[0]**3 + 2*x[0] -2
    g2 = 200*x[1] -200*x[0]**2
    return np.array([g1,g2])

def Hessian_Rosenbrock(x):
    h11 = -400*x[1] + 1200*x[0]**2 + 2
    h12 = -400 * x[0]
    h21 = -400 * x[0]
    h22 = 200
    return np.array([[h11,h12],[h21,h22]])

from scipy.optimize import minimize
x0 = [-1.0,0]
res = minimize(fun=rosenbrock,x0=x0,method='dogleg',jac = Grad_Rosenbrock, hess= Hessian_Rosenbrock)
print (res)
```

```text
     fun: 6.22676428083078e-16
    hess: array([[ 801.9999849, -399.999996 ],
       [-399.999996 ,  200.       ]])
     jac: array([ 8.94341679e-07, -4.57178146e-07])
 message: 'Optimization terminated successfully.'
    nfev: 23
    nhev: 20
     nit: 22
    njev: 21
  status: 0
 success: True
       x: array([0.99999999, 0.99999998])
```
