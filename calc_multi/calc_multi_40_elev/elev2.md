
```python
import sympy

vars = 't a0 a1 a2 a3 b0 b1 b2 b3 gamma x y m1x m1y m2x m2y'
t, a0, a1, a2, a3, b0, b1, b2, b3, gamma, x, y, m1x, m1y, m2x, m2y = sympy.symbols(vars)

tmp1 = sympy.Matrix([[x], [y]])-sympy.Matrix([[m1x], [m1y]])
tmp1 = tmp1.T.dot(tmp1)
tmp2 = sympy.Matrix([[x], [y]])-sympy.Matrix([[m2x], [m2y]])
tmp2 = tmp2.T.dot(tmp2)
f = sympy.Eq(0.5 * sympy.exp(-gamma * tmp1) + 0.5 * sympy.exp(-gamma * tmp2))

xdef = a0 + a1*t + a2*t**2 + a3*t**3 
ydef = b0 + b1*t + b2*t**2 + b3*t**3

dxdt = sympy.diff(xdef,t)
print (dxdt)
dydt = sympy.diff(ydef,t)
print (dydt)
sqrtdef = sympy.sqrt(sympy.diff(xdef,t)**2 + sympy.diff(ydef,t))
print (sqrtdef)
```

```text
a1 + 2*a2*t + 3*a3*t**2
b1 + 2*b2*t + 3*b3*t**2
sqrt(b1 + 2*b2*t + 3*b3*t**2 + (a1 + 2*a2*t + 3*a3*t**2)**2)
```

```python
f2 = f.subs({x: xdef, y: ydef})
f3 = sqrtdef * f2
print (f3)
```

```text
sqrt(b1 + 2*b2*t + 3*b3*t**2 + (a1 + 2*a2*t + 3*a3*t**2)**2)*(Eq(0.5*exp(-gamma*((a0 + a1*t + a2*t**2 + a3*t**3 - m2x)**2 + (b0 + b1*t + b2*t**2 + b3*t**3 - m2y)**2)) + 0.5*exp(-gamma*((a0 + a1*t + a2*t**2 + a3*t**3 - m1x)**2 + (b0 + b1*t + b2*t**2 + b3*t**3 - m1y)**2)), 0))
```

```python
f4 = sympy.integrate(f3, (t, 0, 1))
```








```python
import sympy

vars = 't a0 a1 a2 a3 b0 b1 b2 b3 gamma x m1 m2'
t, a0, a1, a2, a3, b0, b1, b2, b3, gamma, x, m1, m2 = sympy.symbols(vars)

f = sympy.Eq(0.5 * sympy.exp(-gamma * sympy.sqrt((x-m1)**2)) +
             0.5 * sympy.exp(-gamma * sympy.sqrt((x-m2)**2) ))

xdef = a0 + a1*t + a2*t**2 + a3*t**3 
ydef = b0 + b1*t + b2*t**2 + b3*t**3

dxdt = sympy.diff(xdef,t)
print (dxdt)
dydt = sympy.diff(ydef,t)
print (dydt)
```

```text
a1 + 2*a2*t + 3*a3*t**2
b1 + 2*b2*t + 3*b3*t**2
```

```python
f2 = f.subs({x: xdef, y: ydef})
print (f2)
```

```text
Eq(0.5*exp(-gamma*sqrt((a0 + a1*t + a2*t**2 + a3*t**3 - m2)**2)) + 0.5*exp(-gamma*sqrt((a0 + a1*t + a2*t**2 + a3*t**3 - m1)**2)), 0)
```



