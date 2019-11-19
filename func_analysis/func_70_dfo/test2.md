
```python

def Rosenbrock(x,y):
    return (1 + x)**2 + 100*(y - x**2)**2

def Grad_Rosenbrock(x,y):
    g1 = -400*x*y + 400*x**3 + 2*x -2
    g2 = 200*y -200*x**2
    return np.array([g1,g2])

def Hessian_Rosenbrock(x,y):
    h11 = -400*y + 1200*x**2 + 2
    h12 = -400 * x
    h21 = -400 * x
    h22 = 200
    return np.array([[h11,h12],[h21,h22]])

print (Rosenbrock(0.5,2))
print (Grad_Rosenbrock(0.5,2))
print (Hessian_Rosenbrock(0.5,2))

```

```text
308.5
[-351.  350.]
[[-498. -200.]
 [-200.  200.]]
```













