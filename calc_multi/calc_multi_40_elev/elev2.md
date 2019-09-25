
```python
import sympy

vars = 't a0 a1 a2 a3 b0 b1 b2 b3 gamma x m1 m2'
t, a0, a1, a2, a3, b0, b1, b2, b3, gamma, x, m1, m2 = sympy.symbols(vars)

f = sympy.Eq(0.5 * sympy.exp(-gamma * sympy.sqrt((x-m1)**2)) + 0.5 * sympy.exp(-gamma * sympy.sqrt((x-m2)**2) ))

xdef = a0 + a1*t + a2*t**2 
ydef = b0 + b1*t + b2*t**2 

dxdt = sympy.diff(xdef,t)
print (dxdt)
dydt = sympy.diff(ydef,t)
print (dydt)
```

```text
a1 + 2*a2*t
b1 + 2*b2*t
```

```python
f2 = f.subs({x: xdef})
print (f2)
```

```text
Eq(0.5*exp(-gamma*sqrt((a1*t + a2*t**2 + a3 - m2)**2)) + 0.5*exp(-gamma*sqrt((a1*t + a2*t**2 + a3 - m1)**2)), 0)
```



